/*
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
 * misc parsing utilities
 */

#include <time.h>

#include "avstring.h"
#include "avutil.h"
#include "eval.h"
#include "log.h"
#include "random_seed.h"
#include "parseutils.h"

#undef time

#ifdef TEST

#define av_get_random_seed av_get_random_seed_deterministic
static uint32_t av_get_random_seed_deterministic(void);

#define time(t) 1331972053

#endif

int av_parse_ratio(AVRational *q, const char *str, int max,
                   int log_offset, void *log_ctx)
{
    char c;
    int ret;
    int64_t gcd;

    if (sscanf(str, "%d:%d%c", &q->num, &q->den, &c) != 2) {
        double d;
        ret = av_expr_parse_and_eval(&d, str, NULL, NULL,
                                     NULL, NULL, NULL, NULL,
                                     NULL, log_offset, log_ctx);
        if (ret < 0)
            return ret;
        *q = av_d2q(d, max);
    }

    gcd = av_gcd(FFABS(q->num), FFABS(q->den));
    if (gcd) {
        q->num /= gcd;
        q->den /= gcd;
    }

    return 0;
}

typedef struct {
    const char *abbr;
    int width, height;
} VideoSizeAbbr;

typedef struct {
    const char *abbr;
    AVRational rate;
} VideoRateAbbr;

static const VideoSizeAbbr video_size_abbrs[] = {
    { "ntsc",      720, 480 },
    { "pal",       720, 576 },
    { "qntsc",     352, 240 }, /* VCD compliant NTSC */
    { "qpal",      352, 288 }, /* VCD compliant PAL */
    { "sntsc",     640, 480 }, /* square pixel NTSC */
    { "spal",      768, 576 }, /* square pixel PAL */
    { "film",      352, 240 },
    { "ntsc-film", 352, 240 },
    { "sqcif",     128,  96 },
    { "qcif",      176, 144 },
    { "cif",       352, 288 },
    { "4cif",      704, 576 },
    { "16cif",    1408,1152 },
    { "qqvga",     160, 120 },
    { "qvga",      320, 240 },
    { "vga",       640, 480 },
    { "svga",      800, 600 },
    { "xga",      1024, 768 },
    { "uxga",     1600,1200 },
    { "qxga",     2048,1536 },
    { "sxga",     1280,1024 },
    { "qsxga",    2560,2048 },
    { "hsxga",    5120,4096 },
    { "wvga",      852, 480 },
    { "wxga",     1366, 768 },
    { "wsxga",    1600,1024 },
    { "wuxga",    1920,1200 },
    { "woxga",    2560,1600 },
    { "wqsxga",   3200,2048 },
    { "wquxga",   3840,2400 },
    { "whsxga",   6400,4096 },
    { "whuxga",   7680,4800 },
    { "cga",       320, 200 },
    { "ega",       640, 350 },
    { "hd480",     852, 480 },
    { "hd720",    1280, 720 },
    { "hd1080",   1920,1080 },
};

static const VideoRateAbbr video_rate_abbrs[]= {
    { "ntsc",      { 30000, 1001 } },
    { "pal",       {    25,    1 } },
    { "qntsc",     { 30000, 1001 } }, /* VCD compliant NTSC */
    { "qpal",      {    25,    1 } }, /* VCD compliant PAL */
    { "sntsc",     { 30000, 1001 } }, /* square pixel NTSC */
    { "spal",      {    25,    1 } }, /* square pixel PAL */
    { "film",      {    24,    1 } },
    { "ntsc-film", { 24000, 1001 } },
};

int av_parse_video_size(int *width_ptr, int *height_ptr, const char *str)
{
    int i;
    int n = FF_ARRAY_ELEMS(video_size_abbrs);
    const char *p;
    int width = 0, height = 0;

    for (i = 0; i < n; i++) {
        if (!strcmp(video_size_abbrs[i].abbr, str)) {
            width  = video_size_abbrs[i].width;
            height = video_size_abbrs[i].height;
            break;
        }
    }
    if (i == n) {
        p = str;
        width = strtol(p, (void*)&p, 10);
        if (*p)
            p++;
        height = strtol(p, (void*)&p, 10);
    }
    if (width <= 0 || height <= 0)
        return AVERROR(EINVAL);
    *width_ptr  = width;
    *height_ptr = height;
    return 0;
}

int av_parse_video_rate(AVRational *rate, const char *arg)
{
    int i, ret;
    int n = FF_ARRAY_ELEMS(video_rate_abbrs);

    /* First, we check our abbreviation table */
    for (i = 0; i < n; ++i)
        if (!strcmp(video_rate_abbrs[i].abbr, arg)) {
            *rate = video_rate_abbrs[i].rate;
            return 0;
        }

    /* Then, we try to parse it as fraction */
    if ((ret = av_parse_ratio_quiet(rate, arg, 1001000)) < 0)
        return ret;
    if (rate->num <= 0 || rate->den <= 0)
        return AVERROR(EINVAL);
    return 0;
}

typedef struct {
    const char *name;            ///< a string representing the name of the color
    uint8_t     rgb_color[3];    ///< RGB values for the color
} ColorEntry;

static const ColorEntry color_table[] = {
    { "AliceBlue",            { 0xF0, 0xF8, 0xFF } },
    { "AntiqueWhite",         { 0xFA, 0xEB, 0xD7 } },
    { "AntiqueWhite1",        { 0xFF, 0xEF, 0xDB } },
    { "AntiqueWhite2",        { 0xEE, 0xDF, 0xCC } },
    { "AntiqueWhite3",        { 0xCD, 0xC0, 0xB0 } },
    { "AntiqueWhite4",        { 0x8B, 0x83, 0x78 } },
    { "Aqua",                 { 0x00, 0xFF, 0xFF } },
    { "Aquamarine",           { 0x7F, 0xFF, 0xD4 } },
    { "Aquamarine1",          { 0x7F, 0xFF, 0xD4 } },
    { "Aquamarine2",          { 0x76, 0xEE, 0xC6 } },
    { "Aquamarine3",          { 0x66, 0xCD, 0xAA } },
    { "Aquamarine4",          { 0x45, 0x8B, 0x74 } },
    { "Azure",                { 0xF0, 0xFF, 0xFF } },
    { "Azure1",               { 0xF0, 0xFF, 0xFF } },
    { "Azure2",               { 0xE0, 0xEE, 0xEE } },
    { "Azure3",               { 0xC1, 0xCD, 0xCD } },
    { "Azure4",               { 0x83, 0x8B, 0x8B } },
    { "Beige",                { 0xF5, 0xF5, 0xDC } },
    { "Bisque",               { 0xFF, 0xE4, 0xC4 } },
    { "Bisque1",              { 0xFF, 0xE4, 0xC4 } },
    { "Bisque2",              { 0xEE, 0xD5, 0xB7 } },
    { "Bisque3",              { 0xCD, 0xB7, 0x9E } },
    { "Bisque4",              { 0x8B, 0x7D, 0x6B } },
    { "Black",                { 0x00, 0x00, 0x00 } },
    { "BlanchedAlmond",       { 0xFF, 0xEB, 0xCD } },
    { "Blue",                 { 0x00, 0x00, 0xFF } },
    { "Blue1",                { 0x00, 0x00, 0xFF } },
    { "Blue2",                { 0x00, 0x00, 0xEE } },
    { "Blue3",                { 0x00, 0x00, 0xCD } },
    { "Blue4",                { 0x00, 0x00, 0x8B } },
    { "BlueViolet",           { 0x8A, 0x2B, 0xE2 } },
    { "Brown",                { 0xA5, 0x2A, 0x2A } },
    { "Brown1",               { 0xFF, 0x40, 0x40 } },
    { "Brown2",               { 0xEE, 0x3B, 0x3B } },
    { "Brown3",               { 0xCD, 0x33, 0x33 } },
    { "Brown4",               { 0x8B, 0x23, 0x23 } },
    { "BurlyWood",            { 0xDE, 0xB8, 0x87 } },
    { "Burlywood1",           { 0xFF, 0xD3, 0x9B } },
    { "Burlywood2",           { 0xEE, 0xC5, 0x91 } },
    { "Burlywood3",           { 0xCD, 0xAA, 0x7D } },
    { "Burlywood4",           { 0x8B, 0x73, 0x55 } },
    { "CadetBlue",            { 0x5F, 0x9E, 0xA0 } },
    { "CadetBlue1",           { 0x98, 0xF5, 0xFF } },
    { "CadetBlue2",           { 0x8E, 0xE5, 0xEE } },
    { "CadetBlue3",           { 0x7A, 0xC5, 0xCD } },
    { "CadetBlue4",           { 0x53, 0x86, 0x8B } },
    { "Chartreuse",           { 0x7F, 0xFF, 0x00 } },
    { "Chartreuse1",          { 0x7F, 0xFF, 0x00 } },
    { "Chartreuse2",          { 0x76, 0xEE, 0x00 } },
    { "Chartreuse3",          { 0x66, 0xCD, 0x00 } },
    { "Chartreuse4",          { 0x45, 0x8B, 0x00 } },
    { "Chocolate",            { 0xD2, 0x69, 0x1E } },
    { "Chocolate1",           { 0xFF, 0x7F, 0x24 } },
    { "Chocolate2",           { 0xEE, 0x76, 0x21 } },
    { "Chocolate3",           { 0xCD, 0x66, 0x1D } },
    { "Chocolate4",           { 0x8B, 0x45, 0x13 } },
    { "Coral",                { 0xFF, 0x7F, 0x50 } },
    { "Coral1",               { 0xFF, 0x72, 0x56 } },
    { "Coral2",               { 0xEE, 0x6A, 0x50 } },
    { "Coral3",               { 0xCD, 0x5B, 0x45 } },
    { "Coral4",               { 0x8B, 0x3E, 0x2F } },
    { "CornflowerBlue",       { 0x64, 0x95, 0xED } },
    { "Cornsilk",             { 0xFF, 0xF8, 0xDC } },
    { "Cornsilk1",            { 0xFF, 0xF8, 0xDC } },
    { "Cornsilk2",            { 0xEE, 0xE8, 0xCD } },
    { "Cornsilk3",            { 0xCD, 0xC8, 0xB1 } },
    { "Cornsilk4",            { 0x8B, 0x88, 0x78 } },
    { "Crimson",              { 0xDC, 0x14, 0x3C } },
    { "Cyan",                 { 0x00, 0xFF, 0xFF } },
    { "Cyan1",                { 0x00, 0xFF, 0xFF } },
    { "Cyan2",                { 0x00, 0xEE, 0xEE } },
    { "Cyan3",                { 0x00, 0xCD, 0xCD } },
    { "Cyan4",                { 0x00, 0x8B, 0x8B } },
    { "DarkBlue",             { 0x00, 0x00, 0x8B } },
    { "DarkCyan",             { 0x00, 0x8B, 0x8B } },
    { "DarkGoldenRod",        { 0xB8, 0x86, 0x0B } },
    { "DarkGoldenrod1",       { 0xFF, 0xB9, 0x0F } },
    { "DarkGoldenrod2",       { 0xEE, 0xAD, 0x0E } },
    { "DarkGoldenrod3",       { 0xCD, 0x95, 0x0C } },
    { "DarkGoldenrod4",       { 0x8B, 0x65, 0x08 } },
    { "DarkGray",             { 0xA9, 0xA9, 0xA9 } },
    { "DarkGreen",            { 0x00, 0x64, 0x00 } },
    { "DarkGrey",             { 0xA9, 0xA9, 0xA9 } },
    { "DarkKhaki",            { 0xBD, 0xB7, 0x6B } },
    { "DarkMagenta",          { 0x8B, 0x00, 0x8B } },
    { "DarkOliveGreen",       { 0x55, 0x6B, 0x2F } },
    { "DarkOliveGreen1",      { 0xCA, 0xFF, 0x70 } },
    { "DarkOliveGreen2",      { 0xBC, 0xEE, 0x68 } },
    { "DarkOliveGreen3",      { 0xA2, 0xCD, 0x5A } },
    { "DarkOliveGreen4",      { 0x6E, 0x8B, 0x3D } },
    { "DarkOrange",           { 0xFF, 0x8C, 0x00 } },
    { "DarkOrange1",          { 0xFF, 0x7F, 0x00 } },
    { "DarkOrange2",          { 0xEE, 0x76, 0x00 } },
    { "DarkOrange3",          { 0xCD, 0x66, 0x00 } },
    { "DarkOrange4",          { 0x8B, 0x45, 0x00 } },
    { "DarkOrchid",           { 0x99, 0x32, 0xCC } },
    { "DarkOrchid1",          { 0xBF, 0x3E, 0xFF } },
    { "DarkOrchid2",          { 0xB2, 0x3A, 0xEE } },
    { "DarkOrchid3",          { 0x9A, 0x32, 0xCD } },
    { "DarkOrchid4",          { 0x68, 0x22, 0x8B } },
    { "DarkRed",              { 0x8B, 0x00, 0x00 } },
    { "DarkSalmon",           { 0xE9, 0x96, 0x7A } },
    { "DarkSeaGreen",         { 0x8F, 0xBC, 0x8F } },
    { "DarkSeaGreen1",        { 0xC1, 0xFF, 0xC1 } },
    { "DarkSeaGreen2",        { 0xB4, 0xEE, 0xB4 } },
    { "DarkSeaGreen3",        { 0x9B, 0xCD, 0x9B } },
    { "DarkSeaGreen4",        { 0x69, 0x8B, 0x69 } },
    { "DarkSlateBlue",        { 0x48, 0x3D, 0x8B } },
    { "DarkSlateGray",        { 0x2F, 0x4F, 0x4F } },
    { "DarkSlateGray1",       { 0x97, 0xFF, 0xFF } },
    { "DarkSlateGray2",       { 0x8D, 0xEE, 0xEE } },
    { "DarkSlateGray3",       { 0x79, 0xCD, 0xCD } },
    { "DarkSlateGray4",       { 0x52, 0x8B, 0x8B } },
    { "DarkSlateGrey",        { 0x2F, 0x4F, 0x4F } },
    { "DarkTurquoise",        { 0x00, 0xCE, 0xD1 } },
    { "DarkViolet",           { 0x94, 0x00, 0xD3 } },
    { "DeepPink",             { 0xFF, 0x14, 0x93 } },
    { "DeepPink1",            { 0xFF, 0x14, 0x93 } },
    { "DeepPink2",            { 0xEE, 0x12, 0x89 } },
    { "DeepPink3",            { 0xCD, 0x10, 0x76 } },
    { "DeepPink4",            { 0x8B, 0x0A, 0x50 } },
    { "DeepSkyBlue",          { 0x00, 0xBF, 0xFF } },
    { "DeepSkyBlue1",         { 0x00, 0xBF, 0xFF } },
    { "DeepSkyBlue2",         { 0x00, 0xB2, 0xEE } },
    { "DeepSkyBlue3",         { 0x00, 0x9A, 0xCD } },
    { "DeepSkyBlue4",         { 0x00, 0x68, 0x8B } },
    { "DimGray",              { 0x69, 0x69, 0x69 } },
    { "DimGrey",              { 0x69, 0x69, 0x69 } },
    { "DodgerBlue",           { 0x1E, 0x90, 0xFF } },
    { "DodgerBlue1",          { 0x1E, 0x90, 0xFF } },
    { "DodgerBlue2",          { 0x1C, 0x86, 0xEE } },
    { "DodgerBlue3",          { 0x18, 0x74, 0xCD } },
    { "DodgerBlue4",          { 0x10, 0x4E, 0x8B } },
    { "FireBrick",            { 0xB2, 0x22, 0x22 } },
    { "Firebrick1",           { 0xFF, 0x30, 0x30 } },
    { "Firebrick2",           { 0xEE, 0x2C, 0x2C } },
    { "Firebrick3",           { 0xCD, 0x26, 0x26 } },
    { "Firebrick4",           { 0x8B, 0x1A, 0x1A } },
    { "FloralWhite",          { 0xFF, 0xFA, 0xF0 } },
    { "ForestGreen",          { 0x22, 0x8B, 0x22 } },
    { "Fractal",              { 0x80, 0x80, 0x80 } },
    { "Fuchsia",              { 0xFF, 0x00, 0xFF } },
    { "Gainsboro",            { 0xDC, 0xDC, 0xDC } },
    { "GhostWhite",           { 0xF8, 0xF8, 0xFF } },
    { "Gold",                 { 0xFF, 0xD7, 0x00 } },
    { "Gold1",                { 0xFF, 0xD7, 0x00 } },
    { "Gold2",                { 0xEE, 0xC9, 0x00 } },
    { "Gold3",                { 0xCD, 0xAD, 0x00 } },
    { "Gold4",                { 0x8B, 0x75, 0x00 } },
    { "GoldenRod",            { 0xDA, 0xA5, 0x20 } },
    { "Goldenrod1",           { 0xFF, 0xC1, 0x25 } },
    { "Goldenrod2",           { 0xEE, 0xB4, 0x22 } },
    { "Goldenrod3",           { 0xCD, 0x9B, 0x1D } },
    { "Goldenrod4",           { 0x8B, 0x69, 0x14 } },
    { "Gray",                 { 0xBE, 0xBE, 0xBE } },
    { "Gray0",                { 0x00, 0x00, 0x00 } },
    { "Gray1",                { 0x03, 0x03, 0x03 } },
    { "Gray10",               { 0x1A, 0x1A, 0x1A } },
    { "Gray100",              { 0xFF, 0xFF, 0xFF } },
    { "Gray100",              { 0xFF, 0xFF, 0xFF } },
    { "Gray11",               { 0x1C, 0x1C, 0x1C } },
    { "Gray12",               { 0x1F, 0x1F, 0x1F } },
    { "Gray13",               { 0x21, 0x21, 0x21 } },
    { "Gray14",               { 0x24, 0x24, 0x24 } },
    { "Gray15",               { 0x26, 0x26, 0x26 } },
    { "Gray16",               { 0x29, 0x29, 0x29 } },
    { "Gray17",               { 0x2B, 0x2B, 0x2B } },
    { "Gray18",               { 0x2E, 0x2E, 0x2E } },
    { "Gray19",               { 0x30, 0x30, 0x30 } },
    { "Gray2",                { 0x05, 0x05, 0x05 } },
    { "Gray20",               { 0x33, 0x33, 0x33 } },
    { "Gray21",               { 0x36, 0x36, 0x36 } },
    { "Gray22",               { 0x38, 0x38, 0x38 } },
    { "Gray23",               { 0x3B, 0x3B, 0x3B } },
    { "Gray24",               { 0x3D, 0x3D, 0x3D } },
    { "Gray25",               { 0x40, 0x40, 0x40 } },
    { "Gray26",               { 0x42, 0x42, 0x42 } },
    { "Gray27",               { 0x45, 0x45, 0x45 } },
    { "Gray28",               { 0x47, 0x47, 0x47 } },
    { "Gray29",               { 0x4A, 0x4A, 0x4A } },
    { "Gray3",                { 0x08, 0x08, 0x08 } },
    { "Gray30",               { 0x4D, 0x4D, 0x4D } },
    { "Gray31",               { 0x4F, 0x4F, 0x4F } },
    { "Gray32",               { 0x52, 0x52, 0x52 } },
    { "Gray33",               { 0x54, 0x54, 0x54 } },
    { "Gray34",               { 0x57, 0x57, 0x57 } },
    { "Gray35",               { 0x59, 0x59, 0x59 } },
    { "Gray36",               { 0x5C, 0x5C, 0x5C } },
    { "Gray37",               { 0x5E, 0x5E, 0x5E } },
    { "Gray38",               { 0x61, 0x61, 0x61 } },
    { "Gray39",               { 0x63, 0x63, 0x63 } },
    { "Gray4",                { 0x0A, 0x0A, 0x0A } },
    { "Gray40",               { 0x66, 0x66, 0x66 } },
    { "Gray41",               { 0x69, 0x69, 0x69 } },
    { "Gray42",               { 0x6B, 0x6B, 0x6B } },
    { "Gray43",               { 0x6E, 0x6E, 0x6E } },
    { "Gray44",               { 0x70, 0x70, 0x70 } },
    { "Gray45",               { 0x73, 0x73, 0x73 } },
    { "Gray46",               { 0x75, 0x75, 0x75 } },
    { "Gray47",               { 0x78, 0x78, 0x78 } },
    { "Gray48",               { 0x7A, 0x7A, 0x7A } },
    { "Gray49",               { 0x7D, 0x7D, 0x7D } },
    { "Gray5",                { 0x0D, 0x0D, 0x0D } },
    { "Gray50",               { 0x7F, 0x7F, 0x7F } },
    { "Gray51",               { 0x82, 0x82, 0x82 } },
    { "Gray52",               { 0x85, 0x85, 0x85 } },
    { "Gray53",               { 0x87, 0x87, 0x87 } },
    { "Gray54",               { 0x8A, 0x8A, 0x8A } },
    { "Gray55",               { 0x8C, 0x8C, 0x8C } },
    { "Gray56",               { 0x8F, 0x8F, 0x8F } },
    { "Gray57",               { 0x91, 0x91, 0x91 } },
    { "Gray58",               { 0x94, 0x94, 0x94 } },
    { "Gray59",               { 0x96, 0x96, 0x96 } },
    { "Gray6",                { 0x0F, 0x0F, 0x0F } },
    { "Gray60",               { 0x99, 0x99, 0x99 } },
    { "Gray61",               { 0x9C, 0x9C, 0x9C } },
    { "Gray62",               { 0x9E, 0x9E, 0x9E } },
    { "Gray63",               { 0xA1, 0xA1, 0xA1 } },
    { "Gray64",               { 0xA3, 0xA3, 0xA3 } },
    { "Gray65",               { 0xA6, 0xA6, 0xA6 } },
    { "Gray66",               { 0xA8, 0xA8, 0xA8 } },
    { "Gray67",               { 0xAB, 0xAB, 0xAB } },
    { "Gray68",               { 0xAD, 0xAD, 0xAD } },
    { "Gray69",               { 0xB0, 0xB0, 0xB0 } },
    { "Gray7",                { 0x12, 0x12, 0x12 } },
    { "Gray70",               { 0xB3, 0xB3, 0xB3 } },
    { "Gray71",               { 0xB5, 0xB5, 0xB5 } },
    { "Gray72",               { 0xB8, 0xB8, 0xB8 } },
    { "Gray73",               { 0xBA, 0xBA, 0xBA } },
    { "Gray74",               { 0xBD, 0xBD, 0xBD } },
    { "Gray75",               { 0xBF, 0xBF, 0xBF } },
    { "Gray76",               { 0xC2, 0xC2, 0xC2 } },
    { "Gray77",               { 0xC4, 0xC4, 0xC4 } },
    { "Gray78",               { 0xC7, 0xC7, 0xC7 } },
    { "Gray79",               { 0xC9, 0xC9, 0xC9 } },
    { "Gray8",                { 0x14, 0x14, 0x14 } },
    { "Gray80",               { 0xCC, 0xCC, 0xCC } },
    { "Gray81",               { 0xCF, 0xCF, 0xCF } },
    { "Gray82",               { 0xD1, 0xD1, 0xD1 } },
    { "Gray83",               { 0xD4, 0xD4, 0xD4 } },
    { "Gray84",               { 0xD6, 0xD6, 0xD6 } },
    { "Gray85",               { 0xD9, 0xD9, 0xD9 } },
    { "Gray86",               { 0xDB, 0xDB, 0xDB } },
    { "Gray87",               { 0xDE, 0xDE, 0xDE } },
    { "Gray88",               { 0xE0, 0xE0, 0xE0 } },
    { "Gray89",               { 0xE3, 0xE3, 0xE3 } },
    { "Gray9",                { 0x17, 0x17, 0x17 } },
    { "Gray90",               { 0xE5, 0xE5, 0xE5 } },
    { "Gray91",               { 0xE8, 0xE8, 0xE8 } },
    { "Gray92",               { 0xEB, 0xEB, 0xEB } },
    { "Gray93",               { 0xED, 0xED, 0xED } },
    { "Gray94",               { 0xF0, 0xF0, 0xF0 } },
    { "Gray95",               { 0xF2, 0xF2, 0xF2 } },
    { "Gray96",               { 0xF5, 0xF5, 0xF5 } },
    { "Gray97",               { 0xF7, 0xF7, 0xF7 } },
    { "Gray98",               { 0xFA, 0xFA, 0xFA } },
    { "Gray99",               { 0xFC, 0xFC, 0xFC } },
    { "Green",                { 0x00, 0xFF, 0x00 } },
    { "Green1",               { 0x00, 0xFF, 0x00 } },
    { "Green2",               { 0x00, 0xEE, 0x00 } },
    { "Green3",               { 0x00, 0xCD, 0x00 } },
    { "Green4",               { 0x00, 0x8B, 0x00 } },
    { "GreenYellow",          { 0xAD, 0xFF, 0x2F } },
    { "Grey",                 { 0xBE, 0xBE, 0xBE } },
    { "Grey0",                { 0x00, 0x00, 0x00 } },
    { "Grey1",                { 0x03, 0x03, 0x03 } },
    { "Grey10",               { 0x1A, 0x1A, 0x1A } },
    { "Grey100",              { 0xFF, 0xFF, 0xFF } },
    { "Grey11",               { 0x1C, 0x1C, 0x1C } },
    { "Grey12",               { 0x1F, 0x1F, 0x1F } },
    { "Grey13",               { 0x21, 0x21, 0x21 } },
    { "Grey14",               { 0x24, 0x24, 0x24 } },
    { "Grey15",               { 0x26, 0x26, 0x26 } },
    { "Grey16",               { 0x29, 0x29, 0x29 } },
    { "Grey17",               { 0x2B, 0x2B, 0x2B } },
    { "Grey18",               { 0x2E, 0x2E, 0x2E } },
    { "Grey19",               { 0x30, 0x30, 0x30 } },
    { "Grey2",                { 0x05, 0x05, 0x05 } },
    { "Grey20",               { 0x33, 0x33, 0x33 } },
    { "Grey21",               { 0x36, 0x36, 0x36 } },
    { "Grey22",               { 0x38, 0x38, 0x38 } },
    { "Grey23",               { 0x3B, 0x3B, 0x3B } },
    { "Grey24",               { 0x3D, 0x3D, 0x3D } },
    { "Grey25",               { 0x40, 0x40, 0x40 } },
    { "Grey26",               { 0x42, 0x42, 0x42 } },
    { "Grey27",               { 0x45, 0x45, 0x45 } },
    { "Grey28",               { 0x47, 0x47, 0x47 } },
    { "Grey29",               { 0x4A, 0x4A, 0x4A } },
    { "Grey3",                { 0x08, 0x08, 0x08 } },
    { "Grey30",               { 0x4D, 0x4D, 0x4D } },
    { "Grey31",               { 0x4F, 0x4F, 0x4F } },
    { "Grey32",               { 0x52, 0x52, 0x52 } },
    { "Grey33",               { 0x54, 0x54, 0x54 } },
    { "Grey34",               { 0x57, 0x57, 0x57 } },
    { "Grey35",               { 0x59, 0x59, 0x59 } },
    { "Grey36",               { 0x5C, 0x5C, 0x5C } },
    { "Grey37",               { 0x5E, 0x5E, 0x5E } },
    { "Grey38",               { 0x61, 0x61, 0x61 } },
    { "Grey39",               { 0x63, 0x63, 0x63 } },
    { "Grey4",                { 0x0A, 0x0A, 0x0A } },
    { "Grey40",               { 0x66, 0x66, 0x66 } },
    { "Grey41",               { 0x69, 0x69, 0x69 } },
    { "Grey42",               { 0x6B, 0x6B, 0x6B } },
    { "Grey43",               { 0x6E, 0x6E, 0x6E } },
    { "Grey44",               { 0x70, 0x70, 0x70 } },
    { "Grey45",               { 0x73, 0x73, 0x73 } },
    { "Grey46",               { 0x75, 0x75, 0x75 } },
    { "Grey47",               { 0x78, 0x78, 0x78 } },
    { "Grey48",               { 0x7A, 0x7A, 0x7A } },
    { "Grey49",               { 0x7D, 0x7D, 0x7D } },
    { "Grey5",                { 0x0D, 0x0D, 0x0D } },
    { "Grey50",               { 0x7F, 0x7F, 0x7F } },
    { "Grey51",               { 0x82, 0x82, 0x82 } },
    { "Grey52",               { 0x85, 0x85, 0x85 } },
    { "Grey53",               { 0x87, 0x87, 0x87 } },
    { "Grey54",               { 0x8A, 0x8A, 0x8A } },
    { "Grey55",               { 0x8C, 0x8C, 0x8C } },
    { "Grey56",               { 0x8F, 0x8F, 0x8F } },
    { "Grey57",               { 0x91, 0x91, 0x91 } },
    { "Grey58",               { 0x94, 0x94, 0x94 } },
    { "Grey59",               { 0x96, 0x96, 0x96 } },
    { "Grey6",                { 0x0F, 0x0F, 0x0F } },
    { "Grey60",               { 0x99, 0x99, 0x99 } },
    { "Grey61",               { 0x9C, 0x9C, 0x9C } },
    { "Grey62",               { 0x9E, 0x9E, 0x9E } },
    { "Grey63",               { 0xA1, 0xA1, 0xA1 } },
    { "Grey64",               { 0xA3, 0xA3, 0xA3 } },
    { "Grey65",               { 0xA6, 0xA6, 0xA6 } },
    { "Grey66",               { 0xA8, 0xA8, 0xA8 } },
    { "Grey67",               { 0xAB, 0xAB, 0xAB } },
    { "Grey68",               { 0xAD, 0xAD, 0xAD } },
    { "Grey69",               { 0xB0, 0xB0, 0xB0 } },
    { "Grey7",                { 0x12, 0x12, 0x12 } },
    { "Grey70",               { 0xB3, 0xB3, 0xB3 } },
    { "Grey71",               { 0xB5, 0xB5, 0xB5 } },
    { "Grey72",               { 0xB8, 0xB8, 0xB8 } },
    { "Grey73",               { 0xBA, 0xBA, 0xBA } },
    { "Grey74",               { 0xBD, 0xBD, 0xBD } },
    { "Grey75",               { 0xBF, 0xBF, 0xBF } },
    { "Grey76",               { 0xC2, 0xC2, 0xC2 } },
    { "Grey77",               { 0xC4, 0xC4, 0xC4 } },
    { "Grey78",               { 0xC7, 0xC7, 0xC7 } },
    { "Grey79",               { 0xC9, 0xC9, 0xC9 } },
    { "Grey8",                { 0x14, 0x14, 0x14 } },
    { "Grey80",               { 0xCC, 0xCC, 0xCC } },
    { "Grey81",               { 0xCF, 0xCF, 0xCF } },
    { "Grey82",               { 0xD1, 0xD1, 0xD1 } },
    { "Grey83",               { 0xD4, 0xD4, 0xD4 } },
    { "Grey84",               { 0xD6, 0xD6, 0xD6 } },
    { "Grey85",               { 0xD9, 0xD9, 0xD9 } },
    { "Grey86",               { 0xDB, 0xDB, 0xDB } },
    { "Grey87",               { 0xDE, 0xDE, 0xDE } },
    { "Grey88",               { 0xE0, 0xE0, 0xE0 } },
    { "Grey89",               { 0xE3, 0xE3, 0xE3 } },
    { "Grey9",                { 0x17, 0x17, 0x17 } },
    { "Grey90",               { 0xE5, 0xE5, 0xE5 } },
    { "Grey91",               { 0xE8, 0xE8, 0xE8 } },
    { "Grey92",               { 0xEB, 0xEB, 0xEB } },
    { "Grey93",               { 0xED, 0xED, 0xED } },
    { "Grey94",               { 0xF0, 0xF0, 0xF0 } },
    { "Grey95",               { 0xF2, 0xF2, 0xF2 } },
    { "Grey96",               { 0xF5, 0xF5, 0xF5 } },
    { "Grey97",               { 0xF7, 0xF7, 0xF7 } },
    { "Grey98",               { 0xFA, 0xFA, 0xFA } },
    { "Grey99",               { 0xFC, 0xFC, 0xFC } },
    { "HoneyDew",             { 0xF0, 0xFF, 0xF0 } },
    { "Honeydew1",            { 0xF0, 0xFF, 0xF0 } },
    { "Honeydew2",            { 0xE0, 0xEE, 0xE0 } },
    { "Honeydew3",            { 0xC1, 0xCD, 0xC1 } },
    { "Honeydew4",            { 0x83, 0x8B, 0x83 } },
    { "HotPink",              { 0xFF, 0x69, 0xB4 } },
    { "HotPink1",             { 0xFF, 0x6E, 0xB4 } },
    { "HotPink2",             { 0xEE, 0x6A, 0xA7 } },
    { "HotPink3",             { 0xCD, 0x60, 0x90 } },
    { "HotPink4",             { 0x8B, 0x3A, 0x62 } },
    { "IndianRed",            { 0xCD, 0x5C, 0x5C } },
    { "IndianRed1",           { 0xFF, 0x6A, 0x6A } },
    { "IndianRed2",           { 0xEE, 0x63, 0x63 } },
    { "IndianRed3",           { 0xCD, 0x55, 0x55 } },
    { "IndianRed4",           { 0x8B, 0x3A, 0x3A } },
    { "Indigo",               { 0x4B, 0x00, 0x82 } },
    { "Ivory",                { 0xFF, 0xFF, 0xF0 } },
    { "Ivory1",               { 0xFF, 0xFF, 0xF0 } },
    { "Ivory2",               { 0xEE, 0xEE, 0xE0 } },
    { "Ivory3",               { 0xCD, 0xCD, 0xC1 } },
    { "Ivory4",               { 0x8B, 0x8B, 0x83 } },
    { "Khaki",                { 0xF0, 0xE6, 0x8C } },
    { "Khaki1",               { 0xFF, 0xF6, 0x8F } },
    { "Khaki2",               { 0xEE, 0xE6, 0x85 } },
    { "Khaki3",               { 0xCD, 0xC6, 0x73 } },
    { "Khaki4",               { 0x8B, 0x86, 0x4E } },
    { "Lavender",             { 0xE6, 0xE6, 0xFA } },
    { "LavenderBlush",        { 0xFF, 0xF0, 0xF5 } },
    { "LavenderBlush1",       { 0xFF, 0xF0, 0xF5 } },
    { "LavenderBlush2",       { 0xEE, 0xE0, 0xE5 } },
    { "LavenderBlush3",       { 0xCD, 0xC1, 0xC5 } },
    { "LavenderBlush4",       { 0x8B, 0x83, 0x86 } },
    { "LawnGreen",            { 0x7C, 0xFC, 0x00 } },
    { "LemonChiffon",         { 0xFF, 0xFA, 0xCD } },
    { "LemonChiffon1",        { 0xFF, 0xFA, 0xCD } },
    { "LemonChiffon2",        { 0xEE, 0xE9, 0xBF } },
    { "LemonChiffon3",        { 0xCD, 0xC9, 0xA5 } },
    { "LemonChiffon4",        { 0x8B, 0x89, 0x70 } },
    { "LightBlue",            { 0xAD, 0xD8, 0xE6 } },
    { "LightBlue1",           { 0xBF, 0xEF, 0xFF } },
    { "LightBlue2",           { 0xB2, 0xDF, 0xEE } },
    { "LightBlue3",           { 0x9A, 0xC0, 0xCD } },
    { "LightBlue4",           { 0x68, 0x83, 0x8B } },
    { "LightCoral",           { 0xF0, 0x80, 0x80 } },
    { "LightCyan",            { 0xE0, 0xFF, 0xFF } },
    { "LightCyan1",           { 0xE0, 0xFF, 0xFF } },
    { "LightCyan2",           { 0xD1, 0xEE, 0xEE } },
    { "LightCyan3",           { 0xB4, 0xCD, 0xCD } },
    { "LightCyan4",           { 0x7A, 0x8B, 0x8B } },
    { "LightGoldenRod",       { 0xEE, 0xDD, 0x82 } },
    { "LightGoldenRod1",      { 0xFF, 0xEC, 0x8B } },
    { "LightGoldenRod2",      { 0xEE, 0xDC, 0x82 } },
    { "LightGoldenRod3",      { 0xCD, 0xBE, 0x70 } },
    { "LightGoldenRod4",      { 0x8B, 0x81, 0x4C } },
    { "LightGoldenRodYellow", { 0xFA, 0xFA, 0xD2 } },
    { "LightGray",            { 0xD3, 0xD3, 0xD3 } },
    { "LightGreen",           { 0x90, 0xEE, 0x90 } },
    { "LightGrey",            { 0xD3, 0xD3, 0xD3 } },
    { "LightPink",            { 0xFF, 0xB6, 0xC1 } },
    { "LightPink1",           { 0xFF, 0xAE, 0xB9 } },
    { "LightPink2",           { 0xEE, 0xA2, 0xAD } },
    { "LightPink3",           { 0xCD, 0x8C, 0x95 } },
    { "LightPink4",           { 0x8B, 0x5F, 0x65 } },
    { "LightSalmon",          { 0xFF, 0xA0, 0x7A } },
    { "LightSalmon1",         { 0xFF, 0xA0, 0x7A } },
    { "LightSalmon2",         { 0xEE, 0x95, 0x72 } },
    { "LightSalmon3",         { 0xCD, 0x81, 0x62 } },
    { "LightSalmon4",         { 0x8B, 0x57, 0x42 } },
    { "LightSeaGreen",        { 0x20, 0xB2, 0xAA } },
    { "LightSkyBlue",         { 0x87, 0xCE, 0xFA } },
    { "LightSkyBlue1",        { 0xB0, 0xE2, 0xFF } },
    { "LightSkyBlue2",        { 0xA4, 0xD3, 0xEE } },
    { "LightSkyBlue3",        { 0x8D, 0xB6, 0xCD } },
    { "LightSkyBlue4",        { 0x60, 0x7B, 0x8B } },
    { "LightSlateBlue",       { 0x84, 0x70, 0xFF } },
    { "LightSlateGray",       { 0x77, 0x88, 0x99 } },
    { "LightSlateGrey",       { 0x77, 0x88, 0x99 } },
    { "LightSteelBlue",       { 0xB0, 0xC4, 0xDE } },
    { "LightSteelBlue1",      { 0xCA, 0xE1, 0xFF } },
    { "LightSteelBlue2",      { 0xBC, 0xD2, 0xEE } },
    { "LightSteelBlue3",      { 0xA2, 0xB5, 0xCD } },
    { "LightSteelBlue4",      { 0x6E, 0x7B, 0x8B } },
    { "LightYellow",          { 0xFF, 0xFF, 0xE0 } },
    { "LightYellow1",         { 0xFF, 0xFF, 0xE0 } },
    { "LightYellow2",         { 0xEE, 0xEE, 0xD1 } },
    { "LightYellow3",         { 0xCD, 0xCD, 0xB4 } },
    { "LightYellow4",         { 0x8B, 0x8B, 0x7A } },
    { "Lime",                 { 0x00, 0xFF, 0x00 } },
    { "LimeGreen",            { 0x32, 0xCD, 0x32 } },
    { "Linen",                { 0xFA, 0xF0, 0xE6 } },
    { "Magenta",              { 0xFF, 0x00, 0xFF } },
    { "Magenta1",             { 0xFF, 0x00, 0xFF } },
    { "Magenta2",             { 0xEE, 0x00, 0xEE } },
    { "Magenta3",             { 0xCD, 0x00, 0xCD } },
    { "Magenta4",             { 0x8B, 0x00, 0x8B } },
    { "Maroon",               { 0xB0, 0x30, 0x60 } },
    { "Maroon1",              { 0xFF, 0x34, 0xB3 } },
    { "Maroon2",              { 0xEE, 0x30, 0xA7 } },
    { "Maroon3",              { 0xCD, 0x29, 0x90 } },
    { "Maroon4",              { 0x8B, 0x1C, 0x62 } },
    { "MediumAquaMarine",     { 0x66, 0xCD, 0xAA } },
    { "MediumBlue",           { 0x00, 0x00, 0xCD } },
    { "MediumForestGreen",    { 0x32, 0x81, 0x4B } },
    { "MediumGoldenRod",      { 0xD1, 0xC1, 0x66 } },
    { "MediumOrchid",         { 0xBA, 0x55, 0xD3 } },
    { "MediumOrchid1",        { 0xE0, 0x66, 0xFF } },
    { "MediumOrchid2",        { 0xD1, 0x5F, 0xEE } },
    { "MediumOrchid3",        { 0xB4, 0x52, 0xCD } },
    { "MediumOrchid4",        { 0x7A, 0x37, 0x8B } },
    { "MediumPurple",         { 0x93, 0x70, 0xD8 } },
    { "MediumPurple1",        { 0xAB, 0x82, 0xFF } },
    { "MediumPurple2",        { 0x9F, 0x79, 0xEE } },
    { "MediumPurple3",        { 0x89, 0x68, 0xCD } },
    { "MediumPurple4",        { 0x5D, 0x47, 0x8B } },
    { "MediumSeaGreen",       { 0x3C, 0xB3, 0x71 } },
    { "MediumSlateBlue",      { 0x7B, 0x68, 0xEE } },
    { "MediumSpringGreen",    { 0x00, 0xFA, 0x9A } },
    { "MediumTurquoise",      { 0x48, 0xD1, 0xCC } },
    { "MediumVioletRed",      { 0xC7, 0x15, 0x85 } },
    { "MidnightBlue",         { 0x19, 0x19, 0x70 } },
    { "MintCream",            { 0xF5, 0xFF, 0xFA } },
    { "MistyRose",            { 0xFF, 0xE4, 0xE1 } },
    { "MistyRose1",           { 0xFF, 0xE4, 0xE1 } },
    { "MistyRose2",           { 0xEE, 0xD5, 0xD2 } },
    { "MistyRose3",           { 0xCD, 0xB7, 0xB5 } },
    { "MistyRose4",           { 0x8B, 0x7D, 0x7B } },
    { "Moccasin",             { 0xFF, 0xE4, 0xB5 } },
    { "NavajoWhite",          { 0xFF, 0xDE, 0xAD } },
    { "NavajoWhite1",         { 0xFF, 0xDE, 0xAD } },
    { "NavajoWhite2",         { 0xEE, 0xCF, 0xA1 } },
    { "NavajoWhite3",         { 0xCD, 0xB3, 0x8B } },
    { "NavajoWhite4",         { 0x8B, 0x79, 0x5E } },
    { "Navy",                 { 0x00, 0x00, 0x80 } },
    { "NavyBlue",             { 0x00, 0x00, 0x80 } },
    { "OldLace",              { 0xFD, 0xF5, 0xE6 } },
    { "Olive",                { 0x80, 0x80, 0x00 } },
    { "OliveDrab",            { 0x6B, 0x8E, 0x23 } },
    { "OliveDrab1",           { 0xC0, 0xFF, 0x3E } },
    { "OliveDrab2",           { 0xB3, 0xEE, 0x3A } },
    { "OliveDrab3",           { 0x9A, 0xCD, 0x32 } },
    { "OliveDrab4",           { 0x69, 0x8B, 0x22 } },
    { "Orange",               { 0xFF, 0xA5, 0x00 } },
    { "Orange1",              { 0xFF, 0xA5, 0x00 } },
    { "Orange2",              { 0xEE, 0x9A, 0x00 } },
    { "Orange3",              { 0xCD, 0x85, 0x00 } },
    { "Orange4",              { 0x8B, 0x5A, 0x00 } },
    { "OrangeRed",            { 0xFF, 0x45, 0x00 } },
    { "OrangeRed1",           { 0xFF, 0x45, 0x00 } },
    { "OrangeRed2",           { 0xEE, 0x40, 0x00 } },
    { "OrangeRed3",           { 0xCD, 0x37, 0x00 } },
    { "OrangeRed4",           { 0x8B, 0x25, 0x00 } },
    { "Orchid",               { 0xDA, 0x70, 0xD6 } },
    { "Orchid1",              { 0xFF, 0x83, 0xFA } },
    { "Orchid2",              { 0xEE, 0x7A, 0xE9 } },
    { "Orchid3",              { 0xCD, 0x69, 0xC9 } },
    { "Orchid4",              { 0x8B, 0x47, 0x89 } },
    { "PaleGoldenRod",        { 0xEE, 0xE8, 0xAA } },
    { "PaleGreen",            { 0x98, 0xFB, 0x98 } },
    { "PaleGreen1",           { 0x9A, 0xFF, 0x9A } },
    { "PaleGreen2",           { 0x90, 0xEE, 0x90 } },
    { "PaleGreen3",           { 0x7C, 0xCD, 0x7C } },
    { "PaleGreen4",           { 0x54, 0x8B, 0x54 } },
    { "PaleTurquoise",        { 0xAF, 0xEE, 0xEE } },
    { "PaleTurquoise1",       { 0xBB, 0xFF, 0xFF } },
    { "PaleTurquoise2",       { 0xAE, 0xEE, 0xEE } },
    { "PaleTurquoise3",       { 0x96, 0xCD, 0xCD } },
    { "PaleTurquoise4",       { 0x66, 0x8B, 0x8B } },
    { "PaleVioletRed",        { 0xD8, 0x70, 0x93 } },
    { "PaleVioletRed1",       { 0xFF, 0x82, 0xAB } },
    { "PaleVioletRed2",       { 0xEE, 0x79, 0x9F } },
    { "PaleVioletRed3",       { 0xCD, 0x68, 0x89 } },
    { "PaleVioletRed4",       { 0x8B, 0x47, 0x5D } },
    { "PapayaWhip",           { 0xFF, 0xEF, 0xD5 } },
    { "PeachPuff",            { 0xFF, 0xDA, 0xB9 } },
    { "PeachPuff1",           { 0xFF, 0xDA, 0xB9 } },
    { "PeachPuff2",           { 0xEE, 0xCB, 0xAD } },
    { "PeachPuff3",           { 0xCD, 0xAF, 0x95 } },
    { "PeachPuff4",           { 0x8B, 0x77, 0x65 } },
    { "Peru",                 { 0xCD, 0x85, 0x3F } },
    { "Pink",                 { 0xFF, 0xC0, 0xCB } },
    { "Pink1",                { 0xFF, 0xB5, 0xC5 } },
    { "Pink2",                { 0xEE, 0xA9, 0xB8 } },
    { "Pink3",                { 0xCD, 0x91, 0x9E } },
    { "Pink4",                { 0x8B, 0x63, 0x6C } },
    { "Plum",                 { 0xDD, 0xA0, 0xDD } },
    { "Plum1",                { 0xFF, 0xBB, 0xFF } },
    { "Plum2",                { 0xEE, 0xAE, 0xEE } },
    { "Plum3",                { 0xCD, 0x96, 0xCD } },
    { "Plum4",                { 0x8B, 0x66, 0x8B } },
    { "PowderBlue",           { 0xB0, 0xE0, 0xE6 } },
    { "Purple",               { 0xA0, 0x20, 0xF0 } },
    { "Purple1",              { 0x9B, 0x30, 0xFF } },
    { "Purple2",              { 0x91, 0x2C, 0xEE } },
    { "Purple3",              { 0x7D, 0x26, 0xCD } },
    { "Purple4",              { 0x55, 0x1A, 0x8B } },
    { "Red",                  { 0xFF, 0x00, 0x00 } },
    { "Red1",                 { 0xFF, 0x00, 0x00 } },
    { "Red2",                 { 0xEE, 0x00, 0x00 } },
    { "Red3",                 { 0xCD, 0x00, 0x00 } },
    { "Red4",                 { 0x8B, 0x00, 0x00 } },
    { "RosyBrown",            { 0xBC, 0x8F, 0x8F } },
    { "RosyBrown1",           { 0xFF, 0xC1, 0xC1 } },
    { "RosyBrown2",           { 0xEE, 0xB4, 0xB4 } },
    { "RosyBrown3",           { 0xCD, 0x9B, 0x9B } },
    { "RosyBrown4",           { 0x8B, 0x69, 0x69 } },
    { "RoyalBlue",            { 0x41, 0x69, 0xE1 } },
    { "RoyalBlue1",           { 0x48, 0x76, 0xFF } },
    { "RoyalBlue2",           { 0x43, 0x6E, 0xEE } },
    { "RoyalBlue3",           { 0x3A, 0x5F, 0xCD } },
    { "RoyalBlue4",           { 0x27, 0x40, 0x8B } },
    { "SaddleBrown",          { 0x8B, 0x45, 0x13 } },
    { "Salmon",               { 0xFA, 0x80, 0x72 } },
    { "Salmon1",              { 0xFF, 0x8C, 0x69 } },
    { "Salmon2",              { 0xEE, 0x82, 0x62 } },
    { "Salmon3",              { 0xCD, 0x70, 0x54 } },
    { "Salmon4",              { 0x8B, 0x4C, 0x39 } },
    { "SandyBrown",           { 0xF4, 0xA4, 0x60 } },
    { "SeaGreen",             { 0x2E, 0x8B, 0x57 } },
    { "SeaGreen1",            { 0x54, 0xFF, 0x9F } },
    { "SeaGreen2",            { 0x4E, 0xEE, 0x94 } },
    { "SeaGreen3",            { 0x43, 0xCD, 0x80 } },
    { "SeaGreen4",            { 0x2E, 0x8B, 0x57 } },
    { "SeaShell",             { 0xFF, 0xF5, 0xEE } },
    { "SeaShell1",            { 0xFF, 0xF5, 0xEE } },
    { "SeaShell2",            { 0xEE, 0xE5, 0xDE } },
    { "SeaShell3",            { 0xCD, 0xC5, 0xBF } },
    { "SeaShell4",            { 0x8B, 0x86, 0x82 } },
    { "Sienna",               { 0xA0, 0x52, 0x2D } },
    { "Sienna1",              { 0xFF, 0x82, 0x47 } },
    { "Sienna2",              { 0xEE, 0x79, 0x42 } },
    { "Sienna3",              { 0xCD, 0x68, 0x39 } },
    { "Sienna4",              { 0x8B, 0x47, 0x26 } },
    { "Silver",               { 0xC0, 0xC0, 0xC0 } },
    { "SkyBlue",              { 0x87, 0xCE, 0xEB } },
    { "SkyBlue1",             { 0x87, 0xCE, 0xFF } },
    { "SkyBlue2",             { 0x7E, 0xC0, 0xEE } },
    { "SkyBlue3",             { 0x6C, 0xA6, 0xCD } },
    { "SkyBlue4",             { 0x4A, 0x70, 0x8B } },
    { "SlateBlue",            { 0x6A, 0x5A, 0xCD } },
    { "SlateBlue1",           { 0x83, 0x6F, 0xFF } },
    { "SlateBlue2",           { 0x7A, 0x67, 0xEE } },
    { "SlateBlue3",           { 0x69, 0x59, 0xCD } },
    { "SlateBlue4",           { 0x47, 0x3C, 0x8B } },
    { "SlateGray",            { 0x70, 0x80, 0x90 } },
    { "SlateGray1",           { 0xC6, 0xE2, 0xFF } },
    { "SlateGray2",           { 0xB9, 0xD3, 0xEE } },
    { "SlateGray3",           { 0x9F, 0xB6, 0xCD } },
    { "SlateGray4",           { 0x6C, 0x7B, 0x8B } },
    { "SlateGrey",            { 0x70, 0x80, 0x90 } },
    { "Snow",                 { 0xFF, 0xFA, 0xFA } },
    { "Snow1",                { 0xFF, 0xFA, 0xFA } },
    { "Snow2",                { 0xEE, 0xE9, 0xE9 } },
    { "Snow3",                { 0xCD, 0xC9, 0xC9 } },
    { "Snow4",                { 0x8B, 0x89, 0x89 } },
    { "SpringGreen",          { 0x00, 0xFF, 0x7F } },
    { "SpringGreen1",         { 0x00, 0xFF, 0x7F } },
    { "SpringGreen2",         { 0x00, 0xEE, 0x76 } },
    { "SpringGreen3",         { 0x00, 0xCD, 0x66 } },
    { "SpringGreen4",         { 0x00, 0x8B, 0x45 } },
    { "SteelBlue",            { 0x46, 0x82, 0xB4 } },
    { "SteelBlue1",           { 0x63, 0xB8, 0xFF } },
    { "SteelBlue2",           { 0x5C, 0xAC, 0xEE } },
    { "SteelBlue3",           { 0x4F, 0x94, 0xCD } },
    { "SteelBlue4",           { 0x36, 0x64, 0x8B } },
    { "Tan",                  { 0xD2, 0xB4, 0x8C } },
    { "Tan1",                 { 0xFF, 0xA5, 0x4F } },
    { "Tan2",                 { 0xEE, 0x9A, 0x49 } },
    { "Tan3",                 { 0xCD, 0x85, 0x3F } },
    { "Tan4",                 { 0x8B, 0x5A, 0x2B } },
    { "Teal",                 { 0x00, 0x80, 0x80 } },
    { "Thistle",              { 0xD8, 0xBF, 0xD8 } },
    { "Thistle1",             { 0xFF, 0xE1, 0xFF } },
    { "Thistle2",             { 0xEE, 0xD2, 0xEE } },
    { "Thistle3",             { 0xCD, 0xB5, 0xCD } },
    { "Thistle4",             { 0x8B, 0x7B, 0x8B } },
    { "Tomato",               { 0xFF, 0x63, 0x47 } },
    { "Tomato1",              { 0xFF, 0x63, 0x47 } },
    { "Tomato2",              { 0xEE, 0x5C, 0x42 } },
    { "Tomato3",              { 0xCD, 0x4F, 0x39 } },
    { "Tomato4",              { 0x8B, 0x36, 0x26 } },
    { "Turquoise",            { 0x40, 0xE0, 0xD0 } },
    { "Turquoise1",           { 0x00, 0xF5, 0xFF } },
    { "Turquoise2",           { 0x00, 0xE5, 0xEE } },
    { "Turquoise3",           { 0x00, 0xC5, 0xCD } },
    { "Turquoise4",           { 0x00, 0x86, 0x8B } },
    { "Violet",               { 0xEE, 0x82, 0xEE } },
    { "VioletRed",            { 0xD0, 0x20, 0x90 } },
    { "VioletRed1",           { 0xFF, 0x3E, 0x96 } },
    { "VioletRed2",           { 0xEE, 0x3A, 0x8C } },
    { "VioletRed3",           { 0xCD, 0x32, 0x78 } },
    { "VioletRed4",           { 0x8B, 0x22, 0x52 } },
    { "Wheat",                { 0xF5, 0xDE, 0xB3 } },
    { "Wheat1",               { 0xFF, 0xE7, 0xBA } },
    { "Wheat2",               { 0xEE, 0xD8, 0xAE } },
    { "Wheat3",               { 0xCD, 0xBA, 0x96 } },
    { "Wheat4",               { 0x8B, 0x7E, 0x66 } },
    { "White",                { 0xFF, 0xFF, 0xFF } },
    { "WhiteSmoke",           { 0xF5, 0xF5, 0xF5 } },
    { "Yellow",               { 0xFF, 0xFF, 0x00 } },
    { "Yellow1",              { 0xFF, 0xFF, 0x00 } },
    { "Yellow2",              { 0xEE, 0xEE, 0x00 } },
    { "Yellow3",              { 0xCD, 0xCD, 0x00 } },
    { "Yellow4",              { 0x8B, 0x8B, 0x00 } },
    { "YellowGreen",          { 0x9A, 0xCD, 0x32 } },
};

static int color_table_compare(const void *lhs, const void *rhs)
{
    return av_strcasecmp(lhs, ((const ColorEntry *)rhs)->name);
}

#define ALPHA_SEP '@'

int av_parse_color(uint8_t *rgba_color, const char *color_string, int slen,
                   void *log_ctx)
{
    char *tail, color_string2[128];
    const ColorEntry *entry;
    int len, hex_offset = 0;

    if (color_string[0] == '#') {
        hex_offset = 1;
    } else if (!strncmp(color_string, "0x", 2))
        hex_offset = 2;

    if (slen < 0)
        slen = strlen(color_string);
    av_strlcpy(color_string2, color_string + hex_offset,
               FFMIN(slen-hex_offset+1, sizeof(color_string2)));
    if ((tail = strchr(color_string2, ALPHA_SEP)))
        *tail++ = 0;
    len = strlen(color_string2);
    rgba_color[3] = 255;

    if (!av_strcasecmp(color_string2, "random") || !av_strcasecmp(color_string2, "bikeshed")) {
        int rgba = av_get_random_seed();
        rgba_color[0] = rgba >> 24;
        rgba_color[1] = rgba >> 16;
        rgba_color[2] = rgba >> 8;
        rgba_color[3] = rgba;
    } else if (!av_strcasecmp(color_string2, "none")) {
        rgba_color[0] = 0;
        rgba_color[1] = 0;
        rgba_color[2] = 0;
        rgba_color[3] = 0;
    } else if (hex_offset ||
               strspn(color_string2, "0123456789ABCDEFabcdef") == len) {
        char *tail;
        unsigned int rgba = strtoul(color_string2, &tail, 16);

        if (*tail || (len != 6 && len != 8)) {
            av_log(log_ctx, AV_LOG_ERROR, "Invalid 0xRRGGBB[AA] color string: '%s'\n", color_string2);
            return AVERROR(EINVAL);
        }
        if (len == 8) {
            rgba_color[3] = rgba;
            rgba >>= 8;
        }
        rgba_color[0] = rgba >> 16;
        rgba_color[1] = rgba >> 8;
        rgba_color[2] = rgba;
    } else {
        entry = bsearch(color_string2,
                        color_table,
                        FF_ARRAY_ELEMS(color_table),
                        sizeof(ColorEntry),
                        color_table_compare);
        if (!entry) {
            av_log(log_ctx, AV_LOG_ERROR, "Cannot find color '%s'\n", color_string2);
            return AVERROR(EINVAL);
        }
        memcpy(rgba_color, entry->rgb_color, 3);
    }

    if (tail) {
        unsigned long int alpha;
        const char *alpha_string = tail;
        if (!strncmp(alpha_string, "0x", 2)) {
            alpha = strtoul(alpha_string, &tail, 16);
        } else {
            double norm_alpha = strtod(alpha_string, &tail);
            if (norm_alpha < 0.0 || norm_alpha > 1.0)
                alpha = 256;
            else
                alpha = 255 * norm_alpha;
        }

        if (tail == alpha_string || *tail || alpha > 255) {
            av_log(log_ctx, AV_LOG_ERROR, "Invalid alpha value specifier '%s' in '%s'\n",
                   alpha_string, color_string);
            return AVERROR(EINVAL);
        }
        rgba_color[3] = alpha;
    }

    return 0;
}

/* get a positive number between n_min and n_max, for a maximum length
   of len_max. Return -1 if error. */
static int date_get_num(const char **pp,
                        int n_min, int n_max, int len_max)
{
    int i, val, c;
    const char *p;

    p = *pp;
    val = 0;
    for(i = 0; i < len_max; i++) {
        c = *p;
        if (!isdigit(c))
            break;
        val = (val * 10) + c - '0';
        p++;
    }
    /* no number read ? */
    if (p == *pp)
        return -1;
    if (val < n_min || val > n_max)
        return -1;
    *pp = p;
    return val;
}

/**
 * Parse the input string p according to the format string fmt and
 * store its results in the structure dt.
 * This implementation supports only a subset of the formats supported
 * by the standard strptime().
 *
 * @return a pointer to the first character not processed in this
 * function call, or NULL in case the function fails to match all of
 * the fmt string and therefore an error occurred
 */
static const char *small_strptime(const char *p, const char *fmt, struct tm *dt)
{
    int c, val;

    for(;;) {
        c = *fmt++;
        if (c == '\0') {
            return p;
        } else if (c == '%') {
            c = *fmt++;
            switch(c) {
            case 'H':
                val = date_get_num(&p, 0, 23, 2);
                if (val == -1)
                    return NULL;
                dt->tm_hour = val;
                break;
            case 'M':
                val = date_get_num(&p, 0, 59, 2);
                if (val == -1)
                    return NULL;
                dt->tm_min = val;
                break;
            case 'S':
                val = date_get_num(&p, 0, 59, 2);
                if (val == -1)
                    return NULL;
                dt->tm_sec = val;
                break;
            case 'Y':
                val = date_get_num(&p, 0, 9999, 4);
                if (val == -1)
                    return NULL;
                dt->tm_year = val - 1900;
                break;
            case 'm':
                val = date_get_num(&p, 1, 12, 2);
                if (val == -1)
                    return NULL;
                dt->tm_mon = val - 1;
                break;
            case 'd':
                val = date_get_num(&p, 1, 31, 2);
                if (val == -1)
                    return NULL;
                dt->tm_mday = val;
                break;
            case '%':
                goto match;
            default:
                return NULL;
            }
        } else {
        match:
            if (c != *p)
                return NULL;
            p++;
        }
    }
}

time_t av_timegm(struct tm *tm)
{
    time_t t;

    int y = tm->tm_year + 1900, m = tm->tm_mon + 1, d = tm->tm_mday;

    if (m < 3) {
        m += 12;
        y--;
    }

    t = 86400 *
        (d + (153 * m - 457) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 719469);

    t += 3600 * tm->tm_hour + 60 * tm->tm_min + tm->tm_sec;

    return t;
}

int av_parse_time(int64_t *timeval, const char *timestr, int duration)
{
    const char *p, *q;
    int64_t t;
    time_t now;
    struct tm dt = { 0 };
    int today = 0, negative = 0, microseconds = 0;
    int i;
    static const char * const date_fmt[] = {
        "%Y-%m-%d",
        "%Y%m%d",
    };
    static const char * const time_fmt[] = {
        "%H:%M:%S",
        "%H%M%S",
    };

    p = timestr;
    q = NULL;
    *timeval = INT64_MIN;
    if (!duration) {
        now = time(0);

        if (!av_strcasecmp(timestr, "now")) {
            *timeval = (int64_t) now * 1000000;
            return 0;
        }

        /* parse the year-month-day part */
        for (i = 0; i < FF_ARRAY_ELEMS(date_fmt); i++) {
            q = small_strptime(p, date_fmt[i], &dt);
            if (q)
                break;
        }

        /* if the year-month-day part is missing, then take the
         * current year-month-day time */
        if (!q) {
            today = 1;
            q = p;
        }
        p = q;

        if (*p == 'T' || *p == 't' || *p == ' ')
            p++;

        /* parse the hour-minute-second part */
        for (i = 0; i < FF_ARRAY_ELEMS(time_fmt); i++) {
            q = small_strptime(p, time_fmt[i], &dt);
            if (q)
                break;
        }
    } else {
        /* parse timestr as a duration */
        if (p[0] == '-') {
            negative = 1;
            ++p;
        }
        /* parse timestr as HH:MM:SS */
        q = small_strptime(p, time_fmt[0], &dt);
        if (!q) {
            /* parse timestr as S+ */
            dt.tm_sec = strtol(p, (void *)&q, 10);
            if (q == p) /* the parsing didn't succeed */
                return AVERROR(EINVAL);
            dt.tm_min = 0;
            dt.tm_hour = 0;
        }
    }

    /* Now we have all the fields that we can get */
    if (!q)
        return AVERROR(EINVAL);

    /* parse the .m... part */
    if (*q == '.') {
        int n;
        q++;
        for (n = 100000; n >= 1; n /= 10, q++) {
            if (!isdigit(*q))
                break;
            microseconds += n * (*q - '0');
        }
    }

    if (duration) {
        t = dt.tm_hour * 3600 + dt.tm_min * 60 + dt.tm_sec;
    } else {
        int is_utc = *q == 'Z' || *q == 'z';
        q += is_utc;
        if (today) { /* fill in today's date */
            struct tm dt2 = is_utc ? *gmtime(&now) : *localtime(&now);
            dt2.tm_hour = dt.tm_hour;
            dt2.tm_min  = dt.tm_min;
            dt2.tm_sec  = dt.tm_sec;
            dt = dt2;
        }
        t = is_utc ? av_timegm(&dt) : mktime(&dt);
    }

    /* Check that we are at the end of the string */
    if (*q)
        return AVERROR(EINVAL);

    t *= 1000000;
    t += microseconds;
    *timeval = negative ? -t : t;
    return 0;
}

int av_find_info_tag(char *arg, int arg_size, const char *tag1, const char *info)
{
    const char *p;
    char tag[128], *q;

    p = info;
    if (*p == '?')
        p++;
    for(;;) {
        q = tag;
        while (*p != '\0' && *p != '=' && *p != '&') {
            if ((q - tag) < sizeof(tag) - 1)
                *q++ = *p;
            p++;
        }
        *q = '\0';
        q = arg;
        if (*p == '=') {
            p++;
            while (*p != '&' && *p != '\0') {
                if ((q - arg) < arg_size - 1) {
                    if (*p == '+')
                        *q++ = ' ';
                    else
                        *q++ = *p;
                }
                p++;
            }
        }
        *q = '\0';
        if (!strcmp(tag, tag1))
            return 1;
        if (*p != '&')
            break;
        p++;
    }
    return 0;
}

#ifdef TEST

static uint32_t random = MKTAG('L','A','V','U');

static uint32_t av_get_random_seed_deterministic(void)
{
    return random = random * 1664525 + 1013904223;
}

#undef printf

int main(void)
{
    printf("Testing av_parse_video_rate()\n");
    {
        int i;
        const char *rates[] = {
            "-inf",
            "inf",
            "nan",
            "123/0",
            "-123 / 0",
            "",
            "/",
            " 123  /  321",
            "foo/foo",
            "foo/1",
            "1/foo",
            "0/0",
            "/0",
            "1/",
            "1",
            "0",
            "-123/123",
            "-foo",
            "123.23",
            ".23",
            "-.23",
            "-0.234",
            "-0.0000001",
            "  21332.2324   ",
            " -21332.2324   ",
        };

        for (i = 0; i < FF_ARRAY_ELEMS(rates); i++) {
            int ret;
            AVRational q = (AVRational){0, 0};
            ret = av_parse_video_rate(&q, rates[i]);
            printf("'%s' -> %d/%d%s\n",
                   rates[i], q.num, q.den, ret ? " error" : "");
        }
    }

    printf("\nTesting av_parse_color()\n");
    {
        int i;
        uint8_t rgba[4];
        const char *color_names[] = {
            "bikeshed",
            "RaNdOm",
            "foo",
            "red",
            "Red ",
            "RED",
            "Violet",
            "Yellow",
            "Red",
            "0x000000",
            "0x0000000",
            "0xff000000",
            "0x3e34ff",
            "0x3e34ffaa",
            "0xffXXee",
            "0xfoobar",
            "0xffffeeeeeeee",
            "#ff0000",
            "#ffXX00",
            "ff0000",
            "ffXX00",
            "red@foo",
            "random@10",
            "0xff0000@1.0",
            "red@",
            "red@0xfff",
            "red@0xf",
            "red@2",
            "red@0.1",
            "red@-1",
            "red@0.5",
            "red@1.0",
            "red@256",
            "red@10foo",
            "red@-1.0",
            "red@-0.0",
        };

        av_log_set_level(AV_LOG_DEBUG);

        for (i = 0;  i < FF_ARRAY_ELEMS(color_names); i++) {
            if (av_parse_color(rgba, color_names[i], -1, NULL) >= 0)
                printf("%s -> R(%d) G(%d) B(%d) A(%d)\n", color_names[i], rgba[0], rgba[1], rgba[2], rgba[3]);
            else
                printf("%s -> error\n", color_names[i]);
        }
    }

    printf("\nTesting av_parse_time()\n");
    {
        int i;
        int64_t tv;
        time_t tvi;
        struct tm *tm;
        static char tzstr[] = "TZ=CET-1";
        const char *time_string[] = {
            "now",
            "12:35:46",
            "2000-12-20 0:02:47.5z",
            "2000-12-20T010247.6",
        };
        const char *duration_string[] = {
            "2:34:56.79",
            "-1:23:45.67",
            "42.1729",
            "-1729.42",
            "12:34",
        };

        av_log_set_level(AV_LOG_DEBUG);
        putenv(tzstr);
        printf("(now is 2012-03-17 09:14:13 +0100, local time is UTC+1)\n");
        for (i = 0;  i < FF_ARRAY_ELEMS(time_string); i++) {
            printf("%-24s -> ", time_string[i]);
            if (av_parse_time(&tv, time_string[i], 0)) {
                printf("error\n");
            } else {
                tvi = tv / 1000000;
                tm = gmtime(&tvi);
                printf("%14"PRIi64".%06d = %04d-%02d-%02dT%02d:%02d:%02dZ\n",
                       tv / 1000000, (int)(tv % 1000000),
                       tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday,
                       tm->tm_hour, tm->tm_min, tm->tm_sec);
            }
        }
        for (i = 0;  i < FF_ARRAY_ELEMS(duration_string); i++) {
            printf("%-24s -> ", duration_string[i]);
            if (av_parse_time(&tv, duration_string[i], 1)) {
                printf("error\n");
            } else {
                printf("%+21"PRIi64"\n", tv);
            }
        }
    }

    return 0;
}

#endif /* TEST */
