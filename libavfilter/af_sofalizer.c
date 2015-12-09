/*****************************************************************************
 * sofalizer.c : SOFAlizer filter for virtual binaural acoustics
 *****************************************************************************
 * Copyright (C) 2013-2015 Andreas Fuchs, Wolfgang Hrauda,
 *                         Acoustics Research Institute (ARI), Vienna, Austria
 *
 * Authors: Andreas Fuchs <andi.fuchs.mail@gmail.com>
 *          Wolfgang Hrauda <wolfgang.hrauda@gmx.at>
 *
 * SOFAlizer project coordinator at ARI, main developer of SOFA:
 *          Piotr Majdak <piotr@majdak.at>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston MA 02110-1301, USA.
 *****************************************************************************/

#include <math.h>
#include <netcdf.h>

#include "libavutil/opt.h"
#include "avfilter.h"
#include "internal.h"
#include "audio.h"

typedef struct NCSofa {  /* contains data of one SOFA file */
    int ncid;            /* netCDF ID of the opened SOFA file */
    int n_samples;       /* length of one impulse response (IR) */
    int m_dim;           /* number of measurement positions */
    int *data_delay;     /* broadband delay of each IR */
                         /* all measurement positions for each receiver (i.e. ear): */
    float *sp_a;         /* azimuth angles */
    float *sp_e;         /* elevation angles */
    float *sp_r;         /* radii */
                         /* data at each measurement position for each receiver: */
    float *data_ir;      /* IRs (time-domain) */
} NCSofa;

typedef struct SOFAlizerContext {
    const AVClass *class;

    char *filename;             /* name of SOFA file */
    NCSofa sofa;                /* contains data of the SOFA file */

    const int8_t *reorder;      /* reorder in SOFA channel order */
    int sample_rate;            /* sample rate from SOFA file */
    float *speaker_pos;         /* positions of the virtual loudspekaers */
    float gain_lfe;             /* gain applied to LFE channel */

    int n_conv;                 /* number of channels to convolute */

                                /* buffer variables (for convolution) */
    float *ringbuffer_l;        /* buffers input samples, length of one buffer: */
    float *ringbuffer_r;        /* no. input ch. (incl. LFE) x buffer_length */
    int write_l;                /* current write position to ringbuffer */
    int write_r;                /* current write position to ringbuffer */
    int buffer_length;          /* is: longest IR plus max. delay in all SOFA files */
                                /* then choose next power of 2 */

                                /* netCDF variables */
    int *delay_l;               /* broadband delay for each channel/IR to be convolved */
    int *delay_r;
    float *data_ir_l;           /* IRs for all channels to be convolved */
    float *data_ir_r;           /* (this excludes the LFE) */

                         /* control variables */
    float gain;          /* filter gain (in dB) */
    float rotation;      /* rotation of virtual loudspeakers (in degrees)  */
    float elevation;     /* elevation of virtual loudspeakers (in deg.) */
    float radius;        /* distance virtual loudspeakers to listener (in metres) */

    int lfe;             /* whether or not the LFE channel is used */
} SOFAlizerContext;

static int close_sofa(struct NCSofa *sofa)
{
    av_freep(&sofa->data_delay);
    av_freep(&sofa->sp_a);
    av_freep(&sofa->sp_e);
    av_freep(&sofa->sp_r);
    av_freep(&sofa->data_ir);
    nc_close(sofa->ncid);
    sofa->ncid = 0;

    return 0;
}

static int load_sofa(AVFilterContext *ctx, char *filename, int *samplingrate)
{
    struct SOFAlizerContext *s = ctx->priv;
    /* variables associated with content of SOFA file: */
    int ncid, n_dims, n_vars, n_gatts, n_unlim_dim_id, status;
    char data_delay_dim_name[NC_MAX_NAME];
    float *sp_a, *sp_e, *sp_r, *data_ir;
    char *sofa_conventions;
    char dim_name[NC_MAX_NAME];   /* names of netCDF dimensions */
    size_t *dim_length;           /* lengths of netCDF dimensions */
    char *psz_conventions;
    unsigned int sample_rate;
    int data_delay_dim_id[2];
    int samplingrate_id;
    int data_delay_id;
    int n_samples;
    int m_dim_id = -1;
    int n_dim_id = -1;
    int data_ir_id;
    size_t att_len;
    int m_dim;
    int *data_delay;
    int sp_id;
    int i, ret;

    s->sofa.ncid = 0;
    status = nc_open(filename, NC_NOWRITE, &ncid); /* open SOFA file read-only */
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Can't find SOFA-file '%s'\n", filename);
        return AVERROR(EINVAL);
    }

    /* get number of dimensions, vars, global attributes and Id of unlimited dimensions: */
    nc_inq(ncid, &n_dims, &n_vars, &n_gatts, &n_unlim_dim_id);

    /* -- get number of measurements ("M") and length of one IR ("N") -- */
    dim_length = av_malloc_array(n_dims, sizeof(*dim_length));
    if (!dim_length) {
        nc_close(ncid);
        return AVERROR(ENOMEM);
    }

    for (i = 0; i < n_dims; i++) { /* go through all dimensions of file */
        nc_inq_dim(ncid, i, (char *)&dim_name, &dim_length[i]); /* get dimensions */
        if (!strncmp("M", (const char *)&dim_name, 1)) /* get ID of dimension "M" */
            m_dim_id = i;
        if (!strncmp("N", (const char *)&dim_name, 1)) /* get ID of dimension "N" */
            n_dim_id = i;
    }

    if ((m_dim_id == -1) || (n_dim_id == -1)) { /* dimension "M" or "N" couldn't be found */
        av_log(ctx, AV_LOG_ERROR, "Can't find required dimensions in SOFA file.\n");
        av_freep(&dim_length);
        nc_close(ncid);
        return AVERROR(EINVAL);
    }

    n_samples = dim_length[n_dim_id]; /* get number of measurements */
    m_dim     = dim_length[m_dim_id]; /* get length of one IR */

    av_freep(&dim_length);

    /* -- check file type -- */
    /* get length of attritube "Conventions" */
    status = nc_inq_attlen(ncid, NC_GLOBAL, "Conventions", &att_len);
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Can't get length of attribute \"Conventions\".\n");
        nc_close(ncid);
        return AVERROR_INVALIDDATA;
    }

    /* check whether file is SOFA file */
    psz_conventions = av_malloc(att_len + 1);
    if (!psz_conventions) {
        nc_close(ncid);
        return AVERROR(ENOMEM);
    }

    nc_get_att_text(ncid, NC_GLOBAL, "Conventions", psz_conventions);
    *(psz_conventions + att_len) = 0;
    if (strncmp("SOFA", psz_conventions, 4)) {
        av_log(ctx, AV_LOG_ERROR, "Not a SOFA file!\n");
        av_freep(&psz_conventions);
        nc_close(ncid);
        return AVERROR(EINVAL);
    }
    av_freep(&psz_conventions);

    status = nc_inq_attlen(ncid, NC_GLOBAL, "SOFAConventions", &att_len);
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Can't get length of attribute \"SOFAConventions\".\n");
        nc_close(ncid);
        return AVERROR_INVALIDDATA;
    }

    sofa_conventions = av_malloc(att_len + 1);
    if (!sofa_conventions) {
        nc_close(ncid);
        return AVERROR(ENOMEM);
    }

    nc_get_att_text(ncid, NC_GLOBAL, "SOFAConventions", sofa_conventions);
    *(sofa_conventions + att_len) = 0;
    if (strncmp("SimpleFreeFieldHRIR", sofa_conventions, att_len)) {
        av_log(ctx, AV_LOG_ERROR, "Not a SimpleFreeFieldHRIR file!\n");
        av_freep(&sofa_conventions);
        nc_close(ncid);
        return AVERROR(EINVAL);
    }
    av_freep(&sofa_conventions);

    /* -- get sampling rate of HRTFs -- */
    /* read ID, then value */
    status  = nc_inq_varid(ncid, "Data.SamplingRate", &samplingrate_id);
    status += nc_get_var_uint(ncid, samplingrate_id, &sample_rate);
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't read Data.SamplingRate.\n");
        nc_close(ncid);
        return AVERROR(EINVAL);
    }
    *samplingrate = sample_rate; /* remember sampling rate */

    /* -- allocate memory for one value for each measurement position: -- */
    sp_a = s->sofa.sp_a = av_malloc_array(m_dim, sizeof(float));
    sp_e = s->sofa.sp_e = av_malloc_array(m_dim, sizeof(float));
    sp_r = s->sofa.sp_r = av_malloc_array(m_dim, sizeof(float));
    /* delay and IR values required for each ear and measurement position: */
    data_delay = s->sofa.data_delay = av_calloc(m_dim, 2 * sizeof(int));
    data_ir = s->sofa.data_ir = av_malloc_array(m_dim * n_samples, sizeof(float) * 2);

    if (!data_delay || !sp_a || !sp_e || !sp_r || !data_ir) {
        /* if memory could not be allocated */
        close_sofa(&s->sofa);
        return AVERROR(ENOMEM);
    }

    /* get impulse responses (HRTFs): */
    /* get corresponding ID */
    status = nc_inq_varid(ncid, "Data.IR", &data_ir_id);
    status += nc_get_var_float(ncid, data_ir_id, data_ir); /* read and store IRs */
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't read Data.IR!\n");
        ret = AVERROR(EINVAL);
        goto error;
    }

    /* get source positions of the HRTFs in the SOFA file: */
    status  = nc_inq_varid(ncid, "SourcePosition", &sp_id); /* get corresponding ID */
    status += nc_get_vara_float(ncid, sp_id, (size_t[2]){ 0 , 0 } ,
                (size_t[2]){ m_dim, 1}, sp_a); /* read & store azimuth angles */
    status += nc_get_vara_float(ncid, sp_id, (size_t[2]){ 0 , 1 } ,
                (size_t[2]){ m_dim, 1}, sp_e); /* read & store elevation angles */
    status += nc_get_vara_float (ncid, sp_id, (size_t[2]){ 0 , 2 } ,
                (size_t[2]){ m_dim, 1}, sp_r); /* read & store radii */
    if (status != NC_NOERR) { /* if any source position variable coudn't be read */
        av_log(ctx, AV_LOG_ERROR, "Couldn't read SourcePosition.\n");
        ret = AVERROR(EINVAL);
        goto error;
    }

    /* read Data.Delay, check for errors and fit it to data_delay */
    status  = nc_inq_varid(ncid, "Data.Delay", &data_delay_id);
    status += nc_inq_vardimid(ncid, data_delay_id, &data_delay_dim_id[0]);
    status += nc_inq_dimname(ncid, data_delay_dim_id[0], data_delay_dim_name);
    if (status != NC_NOERR) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't read Data.Delay.\n");
        ret = AVERROR(EINVAL);
        goto error;
    }

    /* Data.Delay dimension check */
    /* dimension of Data.Delay is [I R]: */
    if (!strncmp(data_delay_dim_name, "I", 2)) {
        /* check 2 characters to assure string is 0-terminated after "I" */
        int delay[2]; /* delays get from SOFA file: */
        av_log(ctx, AV_LOG_DEBUG, "Data.Delay has dimension [I R]\n");
        status = nc_get_var_int(ncid, data_delay_id, &delay[0]);
        if (status != NC_NOERR) {
            av_log(ctx, AV_LOG_ERROR, "Couldn't read Data.Delay\n");
            ret = AVERROR(EINVAL);
            goto error;
        }
        int *data_delay_r = data_delay + m_dim;
        for (i = 0; i < m_dim; i++) { /* extend given dimension [I R] to [M R] */
            /* assign constant delay value for all measurements to data_delay fields */
            *(data_delay + i)   = delay[0];
            *(data_delay_r + i) = delay[1];
        }
        /* dimension of Data.Delay is [M R] */
    } else if (!strncmp(data_delay_dim_name, "M", 2)) {
        av_log(ctx, AV_LOG_ERROR, "Data.Delay in dimension [M R]\n");
        /* get delays from SOFA file: */
        status = nc_get_var_int(ncid, data_delay_id, data_delay);
        if (status != NC_NOERR) {
            av_log(ctx, AV_LOG_ERROR, "Couldn't read Data.Delay\n");
            ret = AVERROR(EINVAL);
            goto error;
        }
    } else { /* dimension of Data.Delay is neither [I R] nor [M R] */
        av_log(ctx, AV_LOG_ERROR, "Data.Delay does not have the required dimensions [I R] or [M R].\n");
        ret = AVERROR(EINVAL);
        goto error;
    }

    /* save information in SOFA struct: */
    s->sofa.m_dim = m_dim; /* no. measurement positions */
    s->sofa.n_samples = n_samples; /* length on one IR */
    s->sofa.ncid = ncid; /* netCDF ID of SOFA file */
    nc_close(ncid); /* close SOFA file */

    return 0;

error:
    close_sofa(&s->sofa);
    return ret;
}

static const int8_t reorder[18][9] = {
    { 0, -1, -1, -1, -1, -1, -1, -1, -1 },
    { 0,  1, -1, -1, -1, -1, -1, -1, -1 },
    { 0,  1,  2, -1, -1, -1, -1, -1, -1 },
    { 0,  1,  2, -1, -1, -1, -1, -1, -1 },
    { 0,  1,  2,  3, -1, -1, -1, -1, -1 },
    { 0,  1,  2,  3, -1, -1, -1, -1, -1 },
    { 0,  1,  2,  3, -1, -1, -1, -1, -1 },
    { 0,  1,  3,  4,  2, -1, -1, -1, -1 },
    { 0,  1,  3,  4,  2, -1, -1, -1, -1 },
    { 0,  1,  4,  5,  2,  3, -1, -1, -1 },
    { 0,  1,  4,  5,  2,  3, -1, -1, -1 },
    { 0,  1,  5,  6,  4,  2,  3, -1, -1 },
    { 0,  1,  5,  6,  3,  4,  2, -1, -1 },
    { 0,  1,  6,  7,  4,  5,  2,  3, -1 },
    { 0,  1,  2,  3,  4,  5,  6,  7,  8 },
    { 0,  1,  2,  3,  4,  5,  6,  7, -1 },
    { 0,  1,  3,  4,  2,  5, -1, -1, -1 },
    { 0,  1,  4,  5,  2,  6,  3, -1, -1 },
};

static int get_speaker_pos(AVFilterContext *ctx, float *speaker_pos)
{
    struct SOFAlizerContext *s = ctx->priv;
    uint64_t channels_layout = ctx->inputs[0]->channel_layout;
    float pos_temp[9];
    int nb_input_channels = ctx->inputs[0]->channels; /* get no. input channels */
    int n_conv = nb_input_channels;

    if (channels_layout & AV_CH_LOW_FREQUENCY) { /* if LFE is used */
        /* decrease number of channels to be convolved: */
        n_conv = nb_input_channels - 1;
    }

    /* set speaker positions according to input channel configuration: */
    switch (channels_layout) {
    case AV_CH_LAYOUT_MONO:
                            pos_temp[0] = 0;
                            break;
    case AV_CH_LAYOUT_STEREO:
    case AV_CH_LAYOUT_2POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            break;
    case AV_CH_LAYOUT_SURROUND:
    case AV_CH_LAYOUT_3POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 0;
                            break;
    case AV_CH_LAYOUT_2_1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 180;
                            break;
    case AV_CH_LAYOUT_2_2:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 90;
                            pos_temp[3] = 270;
                            break;
    case AV_CH_LAYOUT_QUAD:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 120;
                            pos_temp[3] = 240;
                            break;
    case AV_CH_LAYOUT_4POINT0:
    case AV_CH_LAYOUT_4POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 0;
                            pos_temp[3] = 180;
                            break;
    case AV_CH_LAYOUT_5POINT0:
    case AV_CH_LAYOUT_5POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 90;
                            pos_temp[3] = 270;
                            pos_temp[4] = 0;
                            break;
    case AV_CH_LAYOUT_5POINT0_BACK:
    case AV_CH_LAYOUT_5POINT1_BACK:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 120;
                            pos_temp[3] = 240;
                            pos_temp[4] = 0;
                            break;
    case AV_CH_LAYOUT_6POINT0:
    case AV_CH_LAYOUT_6POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 90;
                            pos_temp[3] = 270;
                            pos_temp[4] = 0;
                            pos_temp[5] = 180;
                            break;
    case AV_CH_LAYOUT_6POINT1_BACK:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 120;
                            pos_temp[3] = 240;
                            pos_temp[4] = 0;
                            pos_temp[4] = 180;
                            break;
    case AV_CH_LAYOUT_HEXAGONAL:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 120;
                            pos_temp[3] = 240;
                            pos_temp[4] = 0;
                            pos_temp[5] = 180;
                            break;
    case AV_CH_LAYOUT_7POINT0:
    case AV_CH_LAYOUT_7POINT1:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 90;
                            pos_temp[3] = 270;
                            pos_temp[4] = 150;
                            pos_temp[5] = 210;
                            pos_temp[6] = 0;
                            break;
    case AV_CH_LAYOUT_OCTAGONAL:
                            pos_temp[0] = 30;
                            pos_temp[1] = 330;
                            pos_temp[2] = 0;
                            pos_temp[3] = 150;
                            pos_temp[4] = 210;
                            pos_temp[5] = 180;
                            pos_temp[6] = 90;
                            pos_temp[7] = 270;
                            break;
    default:
                            return -1;
    }

    switch (channels_layout) {
    case AV_CH_LAYOUT_MONO:
                            s->reorder = reorder[0];
                            break;
    case AV_CH_LAYOUT_STEREO:
                            s->reorder = reorder[1];
                            break;
    case AV_CH_LAYOUT_2_1:
    case AV_CH_LAYOUT_2POINT1:
                            s->reorder = reorder[2];
                            break;
    case AV_CH_LAYOUT_SURROUND:
                            s->reorder = reorder[3];
                            break;
    case AV_CH_LAYOUT_3POINT1:
    case AV_CH_LAYOUT_2_2:
                            s->reorder = reorder[4];
                            break;
    case AV_CH_LAYOUT_QUAD:
                            s->reorder = reorder[5];
                            break;
    case AV_CH_LAYOUT_4POINT0:
                            s->reorder = reorder[6];
                            break;
    case AV_CH_LAYOUT_4POINT1:
                            s->reorder = reorder[7];
                            break;
    case AV_CH_LAYOUT_5POINT0:
    case AV_CH_LAYOUT_5POINT0_BACK:
                            s->reorder = reorder[8];
                            break;
    case AV_CH_LAYOUT_5POINT1:
    case AV_CH_LAYOUT_5POINT1_BACK:
                            s->reorder = reorder[9];
                            break;
    case AV_CH_LAYOUT_6POINT0:
                            s->reorder = reorder[10];
                            break;
    case AV_CH_LAYOUT_HEXAGONAL:
                            s->reorder = reorder[16];
                            break;
    case AV_CH_LAYOUT_6POINT1:
                            s->reorder = reorder[11];
                            break;
    case AV_CH_LAYOUT_6POINT1_BACK:
                            s->reorder = reorder[17];
                            break;
    case AV_CH_LAYOUT_7POINT0:
                            s->reorder = reorder[12];
                            break;
    case AV_CH_LAYOUT_7POINT1:
                            s->reorder = reorder[13];
                            break;
    case AV_CH_LAYOUT_OCTAGONAL:
                            s->reorder = reorder[15];
                            break;
    default:
                            return -1;
    }

    memcpy(speaker_pos, pos_temp, n_conv * sizeof(float));

    return 0;

}

static int max_delay(struct NCSofa *sofa)
{
    int i, max = 0;

    for (i = 0; i < sofa->m_dim * 2; i++) {
        /* search maximum delay in given SOFA file */
        if (*(sofa->data_delay + i) > max)
            max = *(sofa->data_delay + i);
    }

    return max;
}

static int find_m(SOFAlizerContext *s, int azim, int elev, float radius)
{
    /* get source positions and M of currently selected SOFA file */
    float *sp_a = s->sofa.sp_a; /* azimuth angle */
    float *sp_e = s->sofa.sp_e; /* elevation angle */
    float *sp_r = s->sofa.sp_r; /* radius */
    int m_dim = s->sofa.m_dim; /* no. measurements */
    int best_id = 0; /* index m currently closest to desired source pos. */
    float delta = 1000; /* offset between desired and currently best pos. */
    float current;
    int i;

    for (i = 0; i < m_dim; i++) {
        /* search through all measurements in currently selected SOFA file */
        /* distance of current to desired source position: */
        current = fabs(*(sp_a++) - azim) +
                  fabs(*(sp_e++) - elev) +
                  fabs(*(sp_r++) - radius);
        if (current <= delta) {
            /* if current distance is smaller than smallest distance so far */
            delta = current;
            best_id = i; /* remember index */
        }
    }

    return best_id;
}

static int compensate_volume(AVFilterContext *ctx)
{
    struct SOFAlizerContext *s = ctx->priv;
    float compensate;
    float energy = 0;
    float *ir;
    int m, j;

    if (s->sofa.ncid) {
        /* find IR at front center position in the SOFA file (IR closest to 0°,0°,1m) */
        struct NCSofa *sofa = &s->sofa;
        m = find_m(s, 0, 0, 1);
        /* get energy of that IR and compensate volume */
        ir = sofa->data_ir + 2 * m * sofa->n_samples;
        for (j = 0; j < sofa->n_samples; j++) {
            energy += *(ir + j) * *(ir + j);
        }
        compensate = 256 / (sofa->n_samples * sqrt(energy));
        av_log(ctx, AV_LOG_DEBUG, "Compensate-factor: %f\n", compensate);
        ir = sofa->data_ir;
        for (j = 0; j < sofa->n_samples * sofa->m_dim * 2; j++) {
            *(ir + j) *= compensate; /* apply volume compensation to IRs */
        }
    }

    return 0;
}

static void sofalizer_convolute(SOFAlizerContext *s, AVFrame *in, AVFrame *out, int offset,
                                int *write, int *delay, const float *ir, int *n_clippings,
                                float *ringbuffer)
{
    int n_samples = s->sofa.n_samples; /* length of one IR */
    const float *src = (const float *)in->data[0]; /* get pointer to audio input buffer */
    float *dst = (float *)out->data[0]; /* get pointer to audio output buffer */
    int in_channels = in->channels; /* number of input channels */
    /* ring buffer length is: longest IR plus max. delay -> next power of 2 */
    int buffer_length = s->buffer_length;
    /* -1 for AND instead of MODULO (applied to powers of 2): */
    uint32_t modulo = (uint32_t) buffer_length - 1;
    float *buffer[10]; /* holds ringbuffer for each input channel */
    int wr = *write;
    int read;
    int i, j, l;

    dst += offset;
    for (l = 0; l < in_channels; l++) {
        /* get starting address of ringbuffer for each input channel */
        buffer[l] = ringbuffer + l * buffer_length;
    }

    for (i = 0; i < in->nb_samples; i++) {
        const float *temp_ir = ir; /* using same set of IRs for each sample */

        *(dst) = 0;
        for (l = 0; l < in_channels; l++) {
            /* write current input sample to ringbuffer (for each channel) */
            *(buffer[l] + wr) = src[s->reorder[l]];
        }

        /* loop goes through all channels to be convolved (excl. LFE): */
        for (l = 0; l < s->n_conv; l++) {
            const float *bptr = buffer[l];

            /* current read position in ringbuffer: input sample write position
             * - delay for l-th ch. + diff. betw. IR length and buffer length
             * (mod buffer length) */
            read = (wr - *(delay + l) - (n_samples - 1) + buffer_length) & modulo;

            for (j = 0; j < n_samples; j++) { /* go through samples of IR */
                /* multiply signal and IR, and add up the results */
                *dst += *(bptr + ((read++) & modulo)) * *(temp_ir++);
            }
        }
        if (s->lfe) { /* LFE */
            /* apply gain to LFE signal and add to output buffer */
            *dst += *(buffer[s->n_conv] + wr) * s->gain_lfe;
        }
        /* clippings counter */
        if (fabs(*dst) > 1)
            *n_clippings += 1;

        /* move output buffer pointer by +2 to get to next sample of processed channel: */
        dst += 2;
        src += in_channels;
        wr   = (wr + 1) & modulo; /* update ringbuffer write position */
    }

    *write = wr; /* remember write position in ringbuffer for next call */

    return;
}

static int filter_frame(AVFilterLink *inlink, AVFrame *in)
{
    AVFilterContext *ctx = inlink->dst;
    SOFAlizerContext *s = ctx->priv;
    AVFilterLink *outlink = ctx->outputs[0];
    int n_clippings_l = 0;
    int n_clippings_r = 0;
    AVFrame *out;

    out = ff_get_audio_buffer(outlink, in->nb_samples);
    if (!out) {
        av_frame_free(&in);
        return AVERROR(ENOMEM);
    }
    av_frame_copy_props(out, in);

    sofalizer_convolute(s, in, out, 0, &s->write_l,
                        s->delay_l, s->data_ir_l,
                        &n_clippings_l,
                        s->ringbuffer_l);
    sofalizer_convolute(s, in, out, 1, &s->write_r,
                        s->delay_r, s->data_ir_r,
                        &n_clippings_r,
                        s->ringbuffer_r);

    /* display error message if clipping occured */
    if (n_clippings_l + n_clippings_r > 0) {
        av_log(ctx, AV_LOG_WARNING, "%d of %d samples clipped. Please reduce gain.\n",
               n_clippings_l + n_clippings_r, out->nb_samples * 2);
    }

    av_frame_free(&in);
    return ff_filter_frame(outlink, out);
}

static int query_formats(AVFilterContext *ctx)
{
    struct SOFAlizerContext *s = ctx->priv;
    AVFilterFormats *formats = NULL;
    AVFilterChannelLayouts *layouts = NULL;
    int ret, sample_rates[] = { 48000, -1 };
    static const uint64_t channel_layouts[] = { AV_CH_LAYOUT_MONO,
                                                AV_CH_LAYOUT_STEREO,
                                                AV_CH_LAYOUT_2POINT1,
                                                AV_CH_LAYOUT_SURROUND,
                                                AV_CH_LAYOUT_2_1,
                                                AV_CH_LAYOUT_4POINT0,
                                                AV_CH_LAYOUT_QUAD,
                                                AV_CH_LAYOUT_2_2,
                                                AV_CH_LAYOUT_3POINT1,
                                                AV_CH_LAYOUT_5POINT0_BACK,
                                                AV_CH_LAYOUT_5POINT0,
                                                AV_CH_LAYOUT_4POINT1,
                                                AV_CH_LAYOUT_5POINT1_BACK,
                                                AV_CH_LAYOUT_5POINT1,
                                                AV_CH_LAYOUT_6POINT0,
                                                AV_CH_LAYOUT_HEXAGONAL,
                                                AV_CH_LAYOUT_6POINT1,
                                                AV_CH_LAYOUT_6POINT1_BACK,
                                                AV_CH_LAYOUT_7POINT0,
                                                AV_CH_LAYOUT_7POINT1,
                                                AV_CH_LAYOUT_OCTAGONAL,
                                                0, };

    ret = ff_add_format(&formats, AV_SAMPLE_FMT_FLT);
    if (ret)
        return ret;
    ret = ff_set_common_formats(ctx, formats);
    if (ret)
        return ret;

    layouts = ff_make_formatu64_list(channel_layouts);
    if (!layouts)
        return AVERROR(ENOMEM);

    ret = ff_channel_layouts_ref(layouts, &ctx->inputs[0]->out_channel_layouts);
    if (ret)
        return ret;

    layouts = NULL;
    ret = ff_add_channel_layout(&layouts, AV_CH_LAYOUT_STEREO);
    if (ret)
        return ret;

    ret = ff_channel_layouts_ref(layouts, &ctx->outputs[0]->in_channel_layouts);
    if (ret)
        return ret;

    sample_rates[0] = s->sample_rate;
    formats = ff_make_format_list(sample_rates);
    if (!formats)
        return AVERROR(ENOMEM);
    return ff_set_common_samplerates(ctx, formats);
}

static int load_data(AVFilterContext *ctx, int azim, int elev, float radius)
{
    struct SOFAlizerContext *s = ctx->priv;
    const int n_samples = s->sofa.n_samples;
    int n_conv = s->n_conv; /* no. channels to convolve (excl. LFE) */
    int delay_l[10]; /* broadband delay for each IR */
    int delay_r[10];
    int nb_input_channels = ctx->inputs[0]->channels; /* no. input channels */
    float gain_lin = expf((s->gain - 3 * nb_input_channels) / 20 * M_LN10); /* gain - 3dB/channel */
    float *data_ir_l = NULL;
    float *data_ir_r = NULL;
    int offset = 0; /* used for faster pointer arithmetics in for-loop */
    int m[s->n_conv]; /* measurement index m of IR closest to required source positions */
    int i, j, azim_orig = azim;

    if (!s->sofa.ncid) { /* if an invalid SOFA file has been selected */
        av_log(ctx, AV_LOG_ERROR, "Selected SOFA file is invalid. Please select valid SOFA file.\n");
        return AVERROR_INVALIDDATA;
    }

    /* get temporary IR for L and R channel */
    data_ir_l = av_malloc_array(n_conv * n_samples, sizeof(*data_ir_l));
    data_ir_r = av_malloc_array(n_conv * n_samples, sizeof(*data_ir_r));
    if (!data_ir_r || !data_ir_l) {
        av_free(data_ir_l);
        av_free(data_ir_r);
        return AVERROR(ENOMEM);
    }

    for (i = 0; i < s->n_conv; i++) {
        /* load and store IRs and corresponding delays */
        azim = (int)(s->speaker_pos[i] + azim_orig) % 360;
        /* get id of IR closest to desired position */
        m[i] = find_m(s, azim, elev, radius);

        /* load the delays associated with the current IRs */
        delay_l[i] = *(s->sofa.data_delay + 2 * m[i]);
        delay_r[i] = *(s->sofa.data_delay + 2 * m[i] + 1);

        offset = i * n_samples; /* no. samples already written */
        for (j = 0; j < n_samples; j++) {
            /* load reversed IRs of the specified source position
             * sample-by-sample for left and right ear; and apply gain */
            *(data_ir_l + offset + j) = /* left channel */
            *(s->sofa.data_ir + 2 * m[i] * n_samples + n_samples - 1 - j) * gain_lin;
            *(data_ir_r + offset + j) = /* right channel */
            *(s->sofa.data_ir + 2 * m[i] * n_samples + n_samples - 1 - j  + n_samples) * gain_lin;
        }

        av_log(ctx, AV_LOG_DEBUG, "Index: %d, Azimuth: %f, Elevation: %f, Radius: %f of SOFA file.\n",
               m[i], *(s->sofa.sp_a + m[i]), *(s->sofa.sp_e + m[i]), *(s->sofa.sp_r + m[i]));
    }

    /* copy IRs and delays to allocated memory in the SOFAlizerContext struct: */
    memcpy(s->data_ir_l, data_ir_l, sizeof(float) * n_conv * n_samples);
    memcpy(s->data_ir_r, data_ir_r, sizeof(float) * n_conv * n_samples);

    av_free(data_ir_l); /* free temporary IR memory */
    av_free(data_ir_r);

    memcpy(s->delay_l, &delay_l[0], sizeof(int) * s->n_conv);
    memcpy(s->delay_r, &delay_r[0], sizeof(int) * s->n_conv);

    return 0;
}

static av_cold int init(AVFilterContext *ctx)
{
    SOFAlizerContext *s = ctx->priv;
    int ret;

    /* load SOFA file, */
    /* initialize file IDs to 0 before attempting to load SOFA files,
     * this assures that in case of error, only the memory of already
     * loaded files is free'd */
    s->sofa.ncid = 0;
    ret = load_sofa(ctx, s->filename, &s->sample_rate);
    if (ret) {
        /* file loading error */
        av_log(ctx, AV_LOG_ERROR, "Error while loading SOFA file: '%s'\n", s->filename);
    } else { /* no file loading error, resampling not required */
        av_log(ctx, AV_LOG_DEBUG, "File '%s' loaded.\n", s->filename);
    }

    if (ret) {
        av_log(ctx, AV_LOG_ERROR, "No valid SOFA file could be loaded. Please specify valid SOFA file.\n");
        return ret;
    }

    return 0;
}

static inline unsigned clz(unsigned x)
{
    unsigned i = sizeof(x) * 8;

    while (x) {
        x >>= 1;
        i--;
    }

    return i;
}

static int config_input(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    SOFAlizerContext *s = ctx->priv;
    int nb_input_channels = inlink->channels; /* no. input channels */
    int n_max_ir = 0;
    int n_current;
    int n_max = 0;
    int ret;

    /* gain -3 dB per channel, -6 dB to get LFE on a similar level */
    s->gain_lfe = expf((s->gain - 3 * inlink->channels - 6) / 20 * M_LN10);

    if (inlink->channel_layout & AV_CH_LOW_FREQUENCY) { /* if LFE is used */
        s->lfe = 1;
        s->n_conv = nb_input_channels - 1; /* LFE is an input channel but requires no convolution */
    } else /* if LFE is not used */ {
        s->lfe = 0;
        s->n_conv = nb_input_channels;
    }

    /* get size of ringbuffer (longest IR plus max. delay) */
    /* then choose next power of 2 for performance optimization */
    n_current = s->sofa.n_samples + max_delay(&s->sofa);
    if (n_current > n_max) {
        /* length of longest IR plus max. delay (in all SOFA files) */
        n_max = n_current;
        /* length of longest IR (without delay, in all SOFA files) */
        n_max_ir = s->sofa.n_samples;
    }
    /* buffer length is longest IR plus max. delay -> next power of 2
       (32 - count leading zeros gives required exponent)  */
    s->buffer_length = exp2(32 - clz((uint32_t)n_max));

    /* Allocate memory for the impulse responses, delays and the ringbuffers */
    /* size: (longest IR) * (number of channels to convolute), without LFE */
    s->data_ir_l = av_malloc_array(n_max_ir, sizeof(*s->data_ir_l) * s->n_conv);
    s->data_ir_r = av_malloc_array(n_max_ir, sizeof(*s->data_ir_r) * s->n_conv);
    /* length:  number of channels to convolute */
    s->delay_l = av_malloc_array(s->n_conv, sizeof(*s->delay_l));
    s->delay_r = av_malloc_array(s->n_conv, sizeof(*s->delay_r));
    /* length: (buffer length) * (number of input channels),
     * OR: buffer length (if frequency domain processing)
     * calloc zero-initializes the buffer */
    s->ringbuffer_l = av_calloc(s->buffer_length, sizeof(*s->ringbuffer_l) * nb_input_channels);
    s->ringbuffer_r = av_calloc(s->buffer_length, sizeof(*s->ringbuffer_r) * nb_input_channels);
    /* length: number of channels to convolute */
    s->speaker_pos = av_malloc_array(s->n_conv, sizeof(*s->speaker_pos));

    /* memory allocation failed: */
    if (!s->data_ir_l || !s->data_ir_r || !s->delay_l ||
        !s->delay_r || !s->ringbuffer_l || !s->ringbuffer_r ||
        !s->speaker_pos)
        return AVERROR(ENOMEM);

    compensate_volume(ctx);

    /* get speaker positions */
    if ((ret = get_speaker_pos(ctx, s->speaker_pos)) < 0) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't get speaker positions. Input channel configuration not supported.\n");
        return ret;
    }
    /* load IRs to data_ir_l and data_ir_r for required directions */
    /* only load IRs if time-domain convolution is used. */
    if ((ret = load_data(ctx, s->rotation, s->elevation, s->radius)) < 0)
        return ret;

    av_log(ctx, AV_LOG_DEBUG, "Samplerate: %d Channels to convolute: %d, Length of ringbuffer: %d x %d\n",
        inlink->sample_rate, s->n_conv, nb_input_channels, s->buffer_length);

    return 0;
}

static av_cold void uninit(AVFilterContext *ctx)
{
    SOFAlizerContext *s = ctx->priv;

    if (s->sofa.ncid) {
        av_freep(&s->sofa.sp_a);
        av_freep(&s->sofa.sp_e);
        av_freep(&s->sofa.sp_r);
        av_freep(&s->sofa.data_delay);
        av_freep(&s->sofa.data_ir);
    }
    av_freep(&s->delay_l);
    av_freep(&s->delay_r);
    av_freep(&s->data_ir_l);
    av_freep(&s->data_ir_r);
    av_freep(&s->ringbuffer_l);
    av_freep(&s->ringbuffer_r);
    av_freep(&s->speaker_pos);
}

#define OFFSET(x) offsetof(SOFAlizerContext, x)
#define FLAGS AV_OPT_FLAG_AUDIO_PARAM|AV_OPT_FLAG_FILTERING_PARAM

static const AVOption sofalizer_options[] = {
    { "sofa",      "sofa filename",  OFFSET(filename),  AV_OPT_TYPE_STRING, {.str=NULL},            .flags = FLAGS },
    { "gain",      "set gain in dB", OFFSET(gain),      AV_OPT_TYPE_FLOAT,  {.dbl=0},     -20,  40, .flags = FLAGS },
    { "rotation",  "set rotation"  , OFFSET(rotation),  AV_OPT_TYPE_FLOAT,  {.dbl=0},    -360, 360, .flags = FLAGS },
    { "elevation", "set elevation",  OFFSET(elevation), AV_OPT_TYPE_FLOAT,  {.dbl=0},     -90,  90, .flags = FLAGS },
    { "radius",    "set radius",     OFFSET(radius),    AV_OPT_TYPE_FLOAT,  {.dbl=1},       0, 2.1, .flags = FLAGS },
    { NULL }
};

AVFILTER_DEFINE_CLASS(sofalizer);

static const AVFilterPad inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_AUDIO,
        .config_props = config_input,
        .filter_frame = filter_frame,
    },
    { NULL }
};

static const AVFilterPad outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_AUDIO,
    },
    { NULL }
};

AVFilter ff_af_sofalizer = {
    .name          = "sofalizer",
    .description   = NULL_IF_CONFIG_SMALL("SOFAlizer (Spatially Oriented Format for Acoustics)."),
    .priv_size     = sizeof(SOFAlizerContext),
    .priv_class    = &sofalizer_class,
    .init          = init,
    .uninit        = uninit,
    .query_formats = query_formats,
    .inputs        = inputs,
    .outputs       = outputs,
};
