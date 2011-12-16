/*
 * QuickTime RPZA video encoder
 * Copyright (C) 2005 Todd Kirby <doubleshot <at> pacbell.net> and David Adler
 *
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

/**
 * @file
 * QuickTime RPZA video encoder
 * @author Todd Kirby <doubleshot <at> pacbell.net> and David Adler
 *
 * For more information about the RPZA format, visit:
 * http://wiki.multimedia.cx/index.php?title=Apple_RPZA
 */

#include "avcodec.h"
#include "put_bits.h"

typedef struct RpzaContext {
    AVCodecContext *avctx;
    AVFrame current_frame;  ///< buffer for current 24 bpp source frame
    AVFrame prev_frame;     ///< buffer for previous 24 bpp source frame
    PutBitContext pb;       ///< buffer for encoded frame data.
} RpzaContext;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} rgb;

#define SQR(x)                ((x) * (x))

#define GET_CHAN(color, chan) ((color) >> ((chan) * 5) & 0x1F)
#define R(color)              GET_CHAN(color, RED)
#define G(color)              GET_CHAN(color, GREEN)
#define B(color)              GET_CHAN(color, BLUE)

/* 8 bit rounding constants */
#define ROUND_UP        7
#define ROUND_NEAREST   4
#define ROUND_DOWN      0

#define PIXELSTRIDE     3

/* tuning parameters */
#define SIXTEEN_COLOR_THRESH        24
#define SKIP_FRAME_THRESH           13
#define START_ONE_COLOR_THRESH      8
#define CONTINUE_ONE_COLOR_THRESH   0

typedef enum { BLUE, GREEN, RED } channel_offset;

typedef struct {
    int row;
    int col;
    int blocks_per_row;
    int block_height;
    int block_width;
    int total_blocks;
    int block_index;
    int rowstride;
    uint16_t start;
} BlockInfo;

static void get_colors(uint8_t *colorB, uint8_t *colorA, uint8_t color4[4][3])
{
    uint8_t step;

    color4[0][0] = colorB[0];
    color4[0][1] = colorB[1];
    color4[0][2] = colorB[2];

    color4[3][0] = colorA[0];
    color4[3][1] = colorA[1];
    color4[3][2] = colorA[2];

    // red components
    step         = FASTDIV((color4[3][0] - color4[0][0] + 1), 3);
    color4[1][0] = color4[0][0] + step;
    color4[2][0] = color4[3][0] - step;

    // green components
    step         = FASTDIV((color4[3][1] - color4[0][1] + 1), 3);
    color4[1][1] = color4[0][1] + step;
    color4[2][1] = color4[3][1] - step;

    // blue components
    step         = FASTDIV((color4[3][2] - color4[0][2] + 1), 3);
    color4[1][2] = color4[0][2] + step;
    color4[2][2] = color4[3][2] - step;
}

/** Fill BlockInfo struct with information about a 4x4 block of the image. */
static int get_block_info(RpzaContext *s, BlockInfo *bi, int block)
{
    bi->row = block / bi->blocks_per_row;
    bi->col = block % bi->blocks_per_row;

    // test for right edge block
    if (bi->col == bi->blocks_per_row - 1 && (s->avctx->width % 4)) {
        bi->block_width = s->avctx->width % 4;
    } else bi->block_width = 4;

    // test for bottom edge block
    if (bi->row == (s->avctx->height >> 2) && (s->avctx->height % 4)) {
        bi->block_height = s->avctx->height % 4;
    } else bi->block_height = 4;

    return block ? (bi->col * 4 * PIXELSTRIDE) + (bi->row * bi->rowstride * 4) : 0;
}

/**
 * Round a 24 bit rgb value to a 15 bit rgb value. The bias parameter
 * specifies the rounding direction.
 */
static uint16_t round_rgb24_to_rgb555(uint8_t * rgb24, int bias)
{
    uint16_t rgb555 = 0;
    uint32_t r, g, b;

    r = ((uint32_t)rgb24[0] + bias) >> 3;
    g = ((uint32_t)rgb24[1] + bias) >> 3;
    b = ((uint32_t)rgb24[2] + bias) >> 3;

    rgb555 |= (r << 10);
    rgb555 |= (g << 5);
    rgb555 |= (b << 0);

    return rgb555;
}

/**
 * Returns the total difference between two 24 bit color values.
 */
static int diff_colors(uint8_t *colorA, uint8_t *colorB)
{
    int tot;
    tot  = SQR(colorA[0] - colorB[0]);
    tot += SQR(colorA[1] - colorB[1]);
    tot += SQR(colorA[2] - colorB[2]);
    return tot;
}

/**
 * Returns the maximum channel difference between two 24 bit color values.
 */
static int max_component_diff(uint8_t *colorA, uint8_t *colorB)
{
    return FFMAX3(FFABS(colorA[0] - colorB[0]), FFABS(colorA[1] - colorB[1]),
                  FFABS(colorA[2] - colorB[2]));
}

/**
 * Find the channel that has the largest difference between minimum and maximum
 * color values. Put the minimum value in min, maximum in max and the channel
 * in chan.
 */
static void get_max_component_diff(BlockInfo *bi, uint8_t *block_ptr, uint8_t *min,
                                   uint8_t *max, channel_offset *chan)
{
    int x, y;
    uint8_t min_r, max_r, min_g, max_g, min_b, max_b;
    uint8_t r, g, b;

    min_r = min_g = min_b = UINT8_MAX;
    max_r = max_g = max_b = 0;

    // loop through and compare pixels
    for (y = 0; y < bi->block_height; y++) {
        for (x = 0; x < bi->block_width; x++) {
            // TODO:  optimize
            min_r = FFMIN(block_ptr[(x * PIXELSTRIDE) + 2], min_r);
            min_g = FFMIN(block_ptr[(x * PIXELSTRIDE) + 1], min_g);
            min_b = FFMIN(block_ptr[(x * PIXELSTRIDE) + 0], min_b);

            max_r = FFMAX(block_ptr[(x * PIXELSTRIDE) + 2], max_r);
            max_g = FFMAX(block_ptr[(x * PIXELSTRIDE) + 1], max_g);
            max_b = FFMAX(block_ptr[(x * PIXELSTRIDE) + 0], max_b);
        }
        block_ptr += bi->rowstride;
    }

    r = max_r - min_r;
    g = max_g - min_g;
    b = max_b - min_b;

    if (r > g && r > b) {
        *max  = max_r;
        *min  = min_r;
        *chan = RED;
    } else if (g > b && g >= r) {
        *max  = max_g;
        *min  = min_g;
        *chan = GREEN;
    } else {
        *max  = max_b;
        *min  = min_b;
        *chan = BLUE;
    }
}

/**
 * Compare two 4x4 blocks to determine if the total difference between the
 * blocks is greater than the thresh parameter. Returns -1 if difference
 * exceeds threshold or zero otherwise.
 */
static int compare_blocks(uint8_t *block1, uint8_t *block2, BlockInfo *bi, int thresh)
{
    int x, y, diff = 0;
    for (y = 0; y < bi->block_height; y++) {
        for (x = 0; x < bi->block_width; x++) {
            diff = max_component_diff(&block1[x * PIXELSTRIDE], &block2[x * PIXELSTRIDE]);
            if (diff >= thresh)
                return -1;
        }
        block1 += bi->rowstride;
        block2 += bi->rowstride;
    }
    return 0;
}

/**
 * Determine the fit of one channel to another within a 4x4 block. This
 * is used to determine the best palette choices for 4-color encoding.
 */
static int leastsquares(uint8_t *block_ptr, BlockInfo *bi,
                        channel_offset xchannel, channel_offset ychannel,
                        double *slope, double *y_intercept, double *correlation_coef)
{
    double sumx = 0, sumy = 0, sumx2 = 0, sumy2 = 0, sumxy = 0;
    double sumx_sq, sumy_sq, tmp, tmp2;
    int i, j, count;
    uint8_t x, y;

    count = bi->block_height * bi->block_width;

    if (count < 2)
        return -1;

    for (i = 0; i < bi->block_height; i++) {
        for (j = 0; j < bi->block_width; j++) {
            x = block_ptr[j * PIXELSTRIDE + xchannel];
            y = block_ptr[j * PIXELSTRIDE + ychannel];
            sumx  += x;
            sumy  += y;
            sumx2 += x * x;
            sumy2 += y * y;
            sumxy += x * y;
        }
        block_ptr += bi->rowstride;
    }

    sumx_sq = sumx * sumx;
    tmp     = (count * sumx2 - sumx_sq);

    if (tmp == 0)
        return -2;

    sumy_sq      = sumy * sumy;
    tmp2         = count * sumy2 - sumy_sq;
    *slope       = (count * sumxy - sumx * sumy) / tmp;
    *y_intercept = (sumy - (*slope) * sumx) / count;

    if (tmp2 == 0)
         *correlation_coef = 0.0;
    else *correlation_coef = (count * sumxy - sumx * sumy) /
                             ff_sqrt(tmp * tmp2);
    return 0;
}

/**
 * Determine the amount of error in the leastsquares fit.
 */
static int calc_lsq_max_fit_error(uint8_t *block_ptr, BlockInfo *bi,
                                  int min, int max, int tmp_min, int tmp_max,
                                  channel_offset xchannel, channel_offset ychannel)
{
    int i, j, x, y;
    int err;
    int max_err = 0;

    for (i = 0; i < bi->block_height; i++) {
        for (j = 0; j < bi->block_width; j++) {
            int x_inc, lin_y, lin_x;
            x = block_ptr[j * PIXELSTRIDE + xchannel];
            y = block_ptr[j * PIXELSTRIDE + ychannel];

            // calculate x_inc as the 4-color index (0..3)
            x_inc = floor( (x - min) * 3.0 / (max - min) + 0.5);
            x_inc = FFMAX(FFMIN(3, x_inc), 0);

            // calculate lin_y corresponding to x_inc
            lin_y = (int)(tmp_min + (tmp_max - tmp_min) * x_inc / 3.0 + 0.5);

            err = FFABS(lin_y - y);
            if (err > max_err)
                max_err = err;

            // calculate lin_x corresponding to x_inc
            lin_x = (int)(min + (max - min) * x_inc / 3.0 + 0.5);

            err = FFABS(lin_x - x);
            if (err > max_err)
                max_err += err;
        }
        block_ptr += bi->rowstride;
    }

    return max_err;
}

/**
 * Find the closest match to a color within the 4-color palette
 */
static int match_color(uint8_t *color, uint8_t colors[4][3])
{
    int ret = 0, channel, palette_entry, variance;
    int smallest_variance = INT_MAX;
    uint8_t dithered_color[3];
    int range;

    for (channel = 0; channel < 3; channel++) {
        range = FFABS(colors[3][channel] - colors[0][channel]);
        dithered_color[channel] = color[channel];
    }

    for (palette_entry = 0; palette_entry < 4; palette_entry++) {
        variance = diff_colors(dithered_color, colors[palette_entry]);

        if (variance < smallest_variance) {
            smallest_variance = variance;
            ret = palette_entry;
        }
    }
    return ret;
}

/**
 * Encode a block using the 4-color opcode and palette. return number of
 * blocks encoded (until we implement multi-block 4 color runs this will
 * always be 1)
 */
static int encode_four_color_block(uint8_t *min_color, uint8_t *max_color,
                                   PutBitContext *pb, uint8_t *block_ptr, BlockInfo *bi)
{
    int x, y, idx;
    uint8_t color4[4][3];
    uint16_t rounded_max, rounded_min;

    // round min and max wider
    rounded_min = round_rgb24_to_rgb555(min_color, ROUND_DOWN);
    rounded_max = round_rgb24_to_rgb555(max_color, ROUND_UP);

    // put a and b colors
    // encode 4 colors = first 16 bit color with MSB zeroed and...
    put_bits(pb, 16, rounded_max & ~0x8000);
    // ...second 16 bit color with MSB on
    put_bits(pb, 16, rounded_min | 0x8000);

    // scale back up to 24 bit
    min_color[0] = R(rounded_min) << 3;
    min_color[1] = G(rounded_min) << 3;
    min_color[2] = B(rounded_min) << 3;

    max_color[0] = R(rounded_max) << 3;
    max_color[1] = G(rounded_max) << 3;
    max_color[2] = B(rounded_max) << 3;

    get_colors(min_color, max_color, color4);

    for (y = 0; y < 4; y++) {
        for (x = 0; x < 4; x++) {
            idx = match_color(&block_ptr[x * PIXELSTRIDE], color4);
            put_bits(pb, 2, idx);
        }
        block_ptr += bi->rowstride;
    }
    return 1; // num blocks encoded
}

/**
 * Copy a 4x4 block from the current frame buffer to the previous frame buffer
 */
static void update_block_in_prev_frame(const uint8_t *src_pixels, uint8_t *dest_pixels,
                                       const BlockInfo *bi, int block_counter)
{
    int y;

    for (y = 0; y < 4; y++) {
        memcpy (dest_pixels, src_pixels, 4 * PIXELSTRIDE);
        dest_pixels += bi->rowstride;
        src_pixels  += bi->rowstride;
    }
}

static int update_block_stats(BlockInfo *bi, uint8_t *block,
                              uint8_t min_color[3], uint8_t max_color[3], int *total_rgb,
                              int *total_pixels, uint8_t avg_color[3], int first_block)
{
    uint8_t min_color_blk[3], max_color_blk[3];
    uint8_t avg_color_blk[3];
    int x, y, is_in_range;
    int total_rgb_blk[3];
    int total_pixels_blk;
    int threshold;

    if (first_block) {
        min_color[0]  = UINT8_MAX;
        min_color[1]  = UINT8_MAX;
        min_color[2]  = UINT8_MAX;
        max_color[0]  = 0;
        max_color[1]  = 0;
        max_color[2]  = 0;
        total_rgb[0]  = 0;
        total_rgb[1]  = 0;
        total_rgb[2]  = 0;
        *total_pixels = 0;
        threshold     = START_ONE_COLOR_THRESH;
    } else {
        threshold     = CONTINUE_ONE_COLOR_THRESH;
    }

    min_color_blk[0] = min_color[0];
    min_color_blk[1] = min_color[1];
    min_color_blk[2] = min_color[2];
    max_color_blk[0] = max_color[0];
    max_color_blk[1] = max_color[1];
    max_color_blk[2] = max_color[2];
    total_rgb_blk[0] = total_rgb[0];
    total_rgb_blk[1] = total_rgb[1];
    total_rgb_blk[2] = total_rgb[2];
    total_pixels_blk = *total_pixels + bi->block_height * bi->block_width;

    for (y = 0; y < bi->block_height; y++) {
        for (x = 0; x < bi->block_width; x++) {
            total_rgb_blk[0] += block[x * PIXELSTRIDE];
            total_rgb_blk[1] += block[x * PIXELSTRIDE + 1];
            total_rgb_blk[2] += block[x * PIXELSTRIDE + 2];

            min_color_blk[0] = FFMIN(block[x * PIXELSTRIDE], min_color_blk[0]);
            min_color_blk[1] = FFMIN(block[x * PIXELSTRIDE + 1], min_color_blk[1]);
            min_color_blk[2] = FFMIN(block[x * PIXELSTRIDE + 2], min_color_blk[2]);

            max_color_blk[0] = FFMAX(block[x * PIXELSTRIDE], max_color_blk[0]);
            max_color_blk[1] = FFMAX(block[x * PIXELSTRIDE + 1], max_color_blk[1]);
            max_color_blk[2] = FFMAX(block[x * PIXELSTRIDE + 2], max_color_blk[2]);
        }
        block += bi->rowstride;
    }

    avg_color_blk[0] = total_rgb_blk[0] / total_pixels_blk;
    avg_color_blk[1] = total_rgb_blk[1] / total_pixels_blk;
    avg_color_blk[2] = total_rgb_blk[2] / total_pixels_blk;

    is_in_range = (max_color_blk[0] - avg_color_blk[0] <= threshold &&
                   max_color_blk[1] - avg_color_blk[1] <= threshold &&
                   max_color_blk[2] - avg_color_blk[2] <= threshold &&
                   avg_color_blk[0] - min_color_blk[0] <= threshold &&
                   avg_color_blk[1] - min_color_blk[1] <= threshold &&
                   avg_color_blk[2] - min_color_blk[2] <= threshold);

    if (is_in_range) {
        min_color[0]  = min_color_blk[0];
        min_color[1]  = min_color_blk[1];
        min_color[2]  = min_color_blk[2];
        max_color[0]  = max_color_blk[0];
        max_color[1]  = max_color_blk[1];
        max_color[2]  = max_color_blk[2];
        total_rgb[0]  = total_rgb_blk[0];
        total_rgb[1]  = total_rgb_blk[1];
        total_rgb[2]  = total_rgb_blk[2];
        avg_color[0]  = avg_color_blk[0];
        avg_color[1]  = avg_color_blk[1];
        avg_color[2]  = avg_color_blk[2];
        *total_pixels = total_pixels_blk;
    }

    return is_in_range;
}

static void rpza_encode_stream(RpzaContext *s, AVFrame *pict)
{
    BlockInfo bi;
    channel_offset chan;
    uint8_t min = 0, max = 0;
    uint8_t avg_color[3];
    uint8_t min_color[3], max_color[3];
    uint8_t *src_pixels  = (uint8_t *) pict->data[0];
    uint8_t *prev_pixels = (uint8_t *) s->prev_frame.data[0];
    double slope, y_intercept, correlation_coef;
    int block_offset = 0, block_counter = 0;
    int n_blocks, total_blocks, i;
    int total_rgb[3], pixel_count;
    int prev_block_offset;
    int tmp_min, tmp_max;

    total_blocks      = ((s->avctx->width + 3) >> 2) * ((s->avctx->height + 3) >> 2);
    bi.rowstride      = pict->linesize[0];
    bi.blocks_per_row = (s->avctx->width + 3) >> 2;

    while (block_counter < total_blocks) {
        if (!s->current_frame.key_frame && s->current_frame.pict_type == FF_P_TYPE) {
            n_blocks          = 0;
            prev_block_offset = 0;

            while (n_blocks < 32 && block_counter + n_blocks < total_blocks) {
                block_offset = get_block_info(s, &bi, block_counter + n_blocks);

                // Multi-block opcodes cannot span multiple rows.
                // If we're starting a new row, break out and write the opcode
                if (prev_block_offset && block_offset - prev_block_offset > 12)
                   break;

                prev_block_offset = block_offset;
                if (compare_blocks(&prev_pixels[block_offset], &src_pixels[block_offset],
                                   &bi, SKIP_FRAME_THRESH)) {
                    if (n_blocks) {
                        // write skip opcode
                        put_bits(&s->pb, 8, 0x80 | (n_blocks - 1));
                        block_counter += n_blocks;
                        goto post_skip;
                    }
                    break;
                }
                /*
                 * NOTE: we don't update skipped blocks in the previous frame buffer
                 * since they always need to be compared against the first skipped
                 * block to avoid artefacts during gradual fade in/outs.
                 */
                n_blocks++;
            }

            // We're either at the end of the frame or we've reached the maximum
            // of 32 blocks in a run. Write out the run.
            if (n_blocks) {
                // write skip opcode
                put_bits(&s->pb, 8, 0x80 | (n_blocks - 1));
                block_counter += n_blocks;
                continue;
            }
        } else block_offset = get_block_info(s, &bi, block_counter);

post_skip:

        if (update_block_stats(&bi, &src_pixels[block_offset], min_color, max_color,
                               total_rgb, &pixel_count, avg_color, 1)) {
            int first_block_offset = prev_block_offset = block_offset;

            n_blocks = 1;
            // update this block in the previous frame buffer
            update_block_in_prev_frame(&src_pixels[block_offset], &prev_pixels[block_offset],
                                       &bi, block_counter + n_blocks);

            while (n_blocks < 32 && block_counter + n_blocks < total_blocks) {
                block_offset = get_block_info(s, &bi, block_counter + n_blocks);

                // Multi-block opcodes cannot span multiple rows.
                // If we've hit end of a row, break out and write the opcode
                if (block_offset - prev_block_offset > 12)
                    break;

                if (!update_block_stats(&bi, &src_pixels[block_offset], min_color, max_color,
                                        total_rgb, &pixel_count, avg_color, 0))
                    break;

                prev_block_offset = block_offset;
                // update this block in the previous frame buffer
                update_block_in_prev_frame(&src_pixels[block_offset], &prev_pixels[block_offset],
                                           &bi, block_counter + n_blocks);
                n_blocks++;
            }

            // write one color opcode.
            put_bits(&s->pb, 8, 0xa0 | (n_blocks - 1));
            // write color to encode.
            put_bits(&s->pb, 16, round_rgb24_to_rgb555(avg_color, ROUND_NEAREST));
            // skip past the blocks we've just encoded.
            block_counter += n_blocks;
        } else {
            int err = 0;

            // get max component diff for block
            get_max_component_diff(&bi, &src_pixels[block_offset], &min, &max, &chan);

            min_color[0] = 0;
            max_color[0] = 0;
            min_color[1] = 0;
            max_color[1] = 0;
            min_color[2] = 0;
            max_color[2] = 0;

            // run least squares against other two components
            for (i = 0; i < 3; i++) {
                if (i == chan) {
                    min_color[i] = min;
                    max_color[i] = max;
                    continue;
                }

                slope = y_intercept = correlation_coef = 0;
                if (leastsquares(&src_pixels[block_offset], &bi, chan, i,
                                 &slope, &y_intercept, &correlation_coef)) {
                    min_color[i] = src_pixels[block_offset + i];
                    max_color[i] = src_pixels[block_offset + i];
                } else {
                    tmp_min = av_clip((int)(0.5 + min * slope + y_intercept), 0, 255);
                    tmp_max = av_clip((int)(0.5 + max * slope + y_intercept), 0, 255);

                    err = FFMAX(calc_lsq_max_fit_error(&src_pixels[block_offset], &bi,
                                min, max, tmp_min, tmp_max, chan, i), err);

                    min_color[i] = tmp_min;
                    max_color[i] = tmp_max;
                }
            }

            if (err > SIXTEEN_COLOR_THRESH) {
                int x, y;
                uint8_t *row_ptr;
                uint16_t rgb555;

                block_offset = get_block_info(s, &bi, block_counter);
                row_ptr      = &src_pixels[block_offset];

                // encode 16 colors = first 16 bit color with MSB zeroed and...
                rgb555 = round_rgb24_to_rgb555(row_ptr, ROUND_NEAREST);
                put_bits(&s->pb, 16, rgb555 & ~0x8000);
                row_ptr += PIXELSTRIDE;

                // ...second 16 bit color with MSB zeroed.
                rgb555 = round_rgb24_to_rgb555(row_ptr, ROUND_NEAREST);
                put_bits(&s->pb, 16, rgb555 & ~0x8000);
                row_ptr += PIXELSTRIDE;

                // skip first two pixels we just encoded above.
                x = 2;
                for (y = 0; y < 4; y++) {
                    for (; x < 4; x++) {
                        rgb555 = round_rgb24_to_rgb555(row_ptr, ROUND_NEAREST);
                        put_bits(&s->pb, 16, rgb555);
                        row_ptr+= PIXELSTRIDE;
                    }
                    x = 0;
                    row_ptr += bi.rowstride - (4 * PIXELSTRIDE);
                }
                block_counter++;
            } else {
               block_counter += encode_four_color_block(min_color, max_color, &s->pb,
                                                        &src_pixels[block_offset], &bi);
            }
            // Update this block in the previous frame buffer
            update_block_in_prev_frame(&src_pixels[block_offset], &prev_pixels[block_offset],
                                       &bi, block_counter);
        }
   }
   return;
}

static av_cold int rpza_encode_init(AVCodecContext *avctx)
{
    RpzaContext * const s = avctx->priv_data;

    avctx->coded_frame = (AVFrame*)&s->current_frame;
    avctx->coded_frame->pict_type = FF_I_TYPE;
    avctx->coded_frame->key_frame = 1;
    s->avctx = avctx;

    return 0;
}

static int rpza_encode_frame(AVCodecContext *avctx, unsigned char *buf,
                             int buf_size, void *data)
{
    RpzaContext * const s = avctx->priv_data;
    AVFrame * const p = (AVFrame*)&s->current_frame;
    AVFrame *pict = data;
    int encoded_frame_size;

    if(!s->current_frame.data[0])
        avctx->get_buffer(avctx, &s->current_frame);

    init_put_bits(&s->pb, buf+4, buf_size);

    *p = *pict;
    p->pict_type = avctx->frame_number % avctx->gop_size ? FF_P_TYPE : FF_I_TYPE;
    p->key_frame = p->pict_type == FF_I_TYPE;

    if(!s->prev_frame.data[0])
        avctx->get_buffer(avctx, &s->prev_frame);

    rpza_encode_stream(s, p);
    flush_put_bits(&s->pb);

    encoded_frame_size = (put_bits_count(&s->pb) >> 3);

    // write header opcode
    buf[0] = 0xe1; // chunk opcode

    // write chunk length
    buf[1] = encoded_frame_size >> 16;
    buf[2] = encoded_frame_size >> 8;
    buf[3] = encoded_frame_size;

    return encoded_frame_size;
}

static av_cold int rpza_encode_end(AVCodecContext *avctx)
{
    RpzaContext * const s = avctx->priv_data;

    if (s->current_frame.data[0])
        avctx->release_buffer(avctx, &s->current_frame);

    if (s->prev_frame.data[0])
        avctx->release_buffer(avctx, &s->prev_frame);

    return 0;
}

AVCodec ff_rpza_encoder = {
    .name           = "rpza",
    .type           = AVMEDIA_TYPE_VIDEO,
    .id             = CODEC_ID_RPZA,
    .priv_data_size = sizeof(RpzaContext),
    .init           = rpza_encode_init,
    .encode         = rpza_encode_frame,
    .close          = rpza_encode_end,
    .pix_fmts       = (const enum PixelFormat[]) { PIX_FMT_RGB24,
                                                   PIX_FMT_NONE },
    .long_name      = NULL_IF_CONFIG_SMALL("QuickTime video (RPZA)"),
};
