/*
 * Copyright (c) 2016 Paul B Mahol
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

#include <float.h>
#include <stdint.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include "libavutil/attributes.h"
#include "libavutil/avstring.h"
#include "libavutil/avassert.h"
#include "libavutil/opt.h"
#include "libavutil/imgutils.h"
#include "libavutil/internal.h"
#include "libavutil/timestamp.h"
#include "libavformat/avformat.h"
#include "audio.h"
#include "avfilter.h"
#include "buffersink.h"
#include "buffersrc.h"
#include "formats.h"
#include "internal.h"
#include "video.h"

typedef struct FilterContext {
    AVFilterContext *f;
    int nb_inputs;
    int nb_outputs;
    const char *options;
    const char *in[64];
    const char *out[64];
    uint8_t linked_out[64];
    uint8_t linked_in[64];
} FilterContext;

typedef struct ScriptStats {
    uint64_t nb_ins[64];
    uint64_t nb_reqs[64];
    uint64_t nb_outs[64];
} ScriptStats;

typedef struct LuaScriptContext {
    const AVClass *class;

    char *script;

    AVFilterGraph *graph;
    FilterContext fctx[1024];
    AVFilterContext *sink[64];
    AVFilterContext *src[64];
    int fsrcid[64];
    int fsinkid[64];
    int fsrcpad[64];
    int fsinkpad[64];
    int pending[64];
    int eof[64];
    int nbf;
    int graph_configured;
    int prev_oframe_count;

    ScriptStats stats;

    lua_State *L;
} LuaScriptContext;

#define OFFSET(x) offsetof(LuaScriptContext, x)
#define FLAGS AV_OPT_FLAG_FILTERING_PARAM | AV_OPT_FLAG_AUDIO_PARAM | AV_OPT_FLAG_VIDEO_PARAM

static const AVOption luascript_options[]= {
    { "script", "specify the script file", OFFSET(script), AV_OPT_TYPE_STRING, .flags = FLAGS },
    { NULL },
};

static int luascript_push_frames(AVFilterContext *ctx, int out_id)
{
    LuaScriptContext *s = ctx->priv;
    int i, min = INT_MAX, ret = 0;

    while (ret >= 0) {
        AVFrame *frame = av_frame_alloc();

        if (!frame)
            return AVERROR(ENOMEM);

        ret = av_buffersink_get_frame_flags(s->sink[out_id], frame,
                                        ctx->nb_inputs > 0 ? AV_BUFFERSINK_FLAG_NO_REQUEST : 0);
        if (ret == AVERROR(EAGAIN)) {
            av_frame_free(&frame);
            ret = 0;
            goto exit;
        } else if (ret < 0) {
            av_frame_free(&frame);
            return ret;
        }

        av_log(ctx, AV_LOG_DEBUG, "ff_filter_frame on outlink %d\n", out_id);
        s->stats.nb_outs[out_id]++;
        ret = ff_filter_frame(ctx->outputs[out_id], frame);
    }

exit:

    av_log(ctx, AV_LOG_DEBUG, "clearing pending\n");
    for (i = 0; i < ctx->nb_inputs; i++) {
        if (!s->eof[i])
            min = FFMIN(s->pending[i], min);
    }
    if (min > 0) {
        for (i = 0; i < ctx->nb_inputs; i++) {
            s->pending[i] = FFMAX(s->pending[i] - 1, 0);
        }
    }

    return ret;
}

static int luascript_filter_frame(AVFilterLink *inlink, AVFrame *frame)
{
    AVFilterContext *ctx = inlink->dst;
    LuaScriptContext *s = ctx->priv;
    unsigned in_id = FF_INLINK_IDX(inlink);
    int ret;

    av_log(ctx, AV_LOG_DEBUG, "filter_frame on inlink %d\n", in_id);

    s->stats.nb_ins[in_id]++;

    ret = av_buffersrc_add_frame_flags(s->src[in_id], frame,
                                       AV_BUFFERSRC_FLAG_PUSH |
                                       AV_BUFFERSRC_FLAG_KEEP_REF);
    av_frame_free(&frame);
    s->pending[in_id]++;
    return ret;
}

static int link_filters(AVFilterContext *ctx)
{
    LuaScriptContext *s = ctx->priv;
    int ret, i, j, k, l;

    for (i = 0; i < s->nbf; i++) {
        FilterContext *fctx = &s->fctx[i];
        for (j = 0; j < fctx->nb_outputs; j++) {
            for (k = 0; k < s->nbf; k++) {
                FilterContext *fctx2 = &s->fctx[k];
                for (l = 0; l < fctx2->nb_inputs; l++) {
                    if (!strcmp(fctx->out[j], fctx2->in[l])) {
                        ret = avfilter_link(fctx->f, j, fctx2->f, l);
                        if (ret < 0)
                            return ret;
                        av_log(ctx, AV_LOG_DEBUG, "%s->%s\n", fctx->f->name, fctx2->f->name);
                        fctx->linked_out[j] = 1;
                        fctx2->linked_in[l] = 1;
                    }
                }
            }
        }
    }

    return 0;
}

static int link_src(AVFilterLink *inlink)
{
    AVFilterContext *ctx = inlink->dst;
    LuaScriptContext *s = ctx->priv;
    unsigned in_id = FF_INLINK_IDX(inlink);
    AVFilterContext *f = s->fctx[s->fsrcid[in_id]].f;
    int j = s->fsrcpad[in_id];
    uint8_t ch_layout[64];
    const char *src_name;
    AVFilter *src;
    int ret;

    if (inlink->type == AVMEDIA_TYPE_VIDEO)
        src = avfilter_get_by_name("buffer");
    else
        src = avfilter_get_by_name("abuffer");
    if (!src) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't find src filter\n");
        return AVERROR(EINVAL);
    }

    src_name = av_asprintf("src%d", in_id);
    s->src[in_id] = avfilter_graph_alloc_filter(s->graph, src, src_name);
    av_freep(&src_name);
    if (!s->src[in_id]) {
        av_log(ctx, AV_LOG_ERROR, "Error allocating src filter\n");
        return AVERROR(ENOMEM);
    }

    if (inlink->type == AVMEDIA_TYPE_VIDEO) {
        av_opt_set_int(s->src[in_id], "width",          inlink->w,                              AV_OPT_SEARCH_CHILDREN);
        av_opt_set_int(s->src[in_id], "height",         inlink->h,                              AV_OPT_SEARCH_CHILDREN);
        av_opt_set_q  (s->src[in_id], "time_base",      inlink->time_base,                      AV_OPT_SEARCH_CHILDREN);
        av_opt_set_int(s->src[in_id], "pix_fmt",        inlink->format,                         AV_OPT_SEARCH_CHILDREN);
        av_opt_set_q  (s->src[in_id], "sar",            inlink->sample_aspect_ratio,            AV_OPT_SEARCH_CHILDREN);
    } else {
        av_get_channel_layout_string(ch_layout, sizeof(ch_layout), 0, inlink->channel_layout);
        av_opt_set    (s->src[in_id], "channel_layout", ch_layout,                              AV_OPT_SEARCH_CHILDREN);
        av_opt_set    (s->src[in_id], "sample_fmt",     av_get_sample_fmt_name(inlink->format), AV_OPT_SEARCH_CHILDREN);
        av_opt_set_q  (s->src[in_id], "time_base",      (AVRational){ 1, inlink->sample_rate }, AV_OPT_SEARCH_CHILDREN);
        av_opt_set_int(s->src[in_id], "sample_rate",    inlink->sample_rate,                    AV_OPT_SEARCH_CHILDREN);
    }

    ret = avfilter_init_str(s->src[in_id], NULL);
    if (ret < 0) {
        return ret;
    }

    ret = avfilter_link(s->src[in_id], 0, f, j);
    if (ret < 0) {
        return ret;
    }

    return 0;
}

static int link_sink(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    unsigned out_id = FF_OUTLINK_IDX(outlink);
    LuaScriptContext *s  = ctx->priv;
    AVFilterContext *f = s->fctx[s->fsinkid[out_id]].f;
    int j = s->fsinkpad[out_id];
    const char *sink_name;
    AVFilter *sink;
    int ret;

    if (outlink->type == AVMEDIA_TYPE_VIDEO)
        sink = avfilter_get_by_name("buffersink");
    else
        sink = avfilter_get_by_name("abuffersink");
    if (!sink) {
        av_log(ctx, AV_LOG_ERROR, "Couldn't find sink filter\n");
        return AVERROR(EINVAL);
    }

    sink_name = av_asprintf("out%d", out_id);
    s->sink[out_id] = avfilter_graph_alloc_filter(s->graph, sink, sink_name);
    av_freep(&sink_name);
    if (!s->sink[out_id]) {
        av_log(ctx, AV_LOG_ERROR, "Error allocating sink filter\n");
        return AVERROR(ENOMEM);
    }

    ret = avfilter_init_str(s->sink[out_id], NULL);
    if (ret < 0) {
        return ret;
    }

    ret = avfilter_link(f, j, s->sink[out_id], 0);
    if (ret < 0) {
        return ret;
    }

    return 0;
}

static AVFilterContext *get_ctx(lua_State *L)
{
    lua_getfield(L, LUA_REGISTRYINDEX, "ctx");
    AVFilterContext *ctx = lua_touserdata(L, -1);
    lua_pop(L, 1);
    av_assert0(ctx);
    return ctx;
}

static int error_handler(lua_State *L)
{
    AVFilterContext *ctx = get_ctx(L);

    if (luaL_loadstring(L, "return debug.traceback('', 3)") == 0) {
        lua_call(L, 0, 1);
        const char *tr = lua_tostring(L, -1);
        av_log(ctx, AV_LOG_ERROR, "%s\n", tr ? tr : "(unknown)");
    }
    lua_pop(L, 1);

    return 1;
}

static int load_script(lua_State *L)
{
    AVFilterContext *ctx = get_ctx(L);
    LuaScriptContext *s = ctx->priv;
    int ret;

    ret = luaL_loadfile(L, s->script);
    if (ret) {
        lua_error(L);
    }
    lua_call(L, 0, 0);

    return ret;
}

static void add_inpad(FilterContext *fctx, const char *in)
{
    fctx->in[fctx->nb_inputs] = in;
    fctx->nb_inputs++;
}

static void add_outpad(FilterContext *fctx, const char *out)
{
    fctx->out[fctx->nb_outputs] = out;
    fctx->nb_outputs++;
}

static int script_filter(lua_State *L)
{
    AVFilterContext *ctx = get_ctx(L);
    LuaScriptContext *s = ctx->priv;
    const int inputs = lua_isnil(L, 1) ? 0 : lua_istable(L, 1);
    const char *name = luaL_checkstring(L, 2);
    const char *options = lua_isnil(L, 3) ? NULL : luaL_checkstring(L, 3);
    const int outputs = lua_isnil(L, 4) ? 0 : lua_istable(L, 4);
    const char *fname;
    int ret = 0;

    if (inputs > 0) {
        lua_pushnil(L);
        while (lua_next(L, 1) != 0) {
            const char *in = luaL_checkstring(L, -1);
            add_inpad(&s->fctx[s->nbf], in);
            lua_pop(L, 1);
        }
    }

    if (outputs > 0) {
        lua_pushnil(L);
        while (lua_next(L, 4) != 0) {
            const char *out = luaL_checkstring(L, -1);
            add_outpad(&s->fctx[s->nbf], out);
            lua_pop(L, 1);
        }
    }

    AVFilter *filter = avfilter_get_by_name(name);
    if (!filter) {
        luaL_error(L, "No such filter %s.", name);
        return AVERROR(EINVAL);
    }

    fname = av_asprintf("%s%d", name, s->nbf);
    s->fctx[s->nbf].f = avfilter_graph_alloc_filter(s->graph, filter, fname);
    av_freep(&fname);
    if (!s->fctx[s->nbf].f)
        return luaL_error(L, "Failed to alloc filter: %s.", name);
    ret = avfilter_init_str(s->fctx[s->nbf].f, options);
    if (ret) {
        luaL_error(L, "Failed to init filter: %s.", name);
        return ret;
    }

    if (s->fctx[s->nbf].f->nb_inputs != s->fctx[s->nbf].nb_inputs) {
        luaL_error(L, "Number of input pads provided does not match filter's: %s.", name);
        ret = AVERROR(EINVAL);
    }
    if (s->fctx[s->nbf].f->nb_outputs != s->fctx[s->nbf].nb_outputs) {
        luaL_error(L, "Number of output pads provided does not match filter's: %s.", name);
        ret = AVERROR(EINVAL);
    }

    s->fctx[s->nbf].options = options;
    s->nbf++;
    return ret;
}

static int script_frame_count(lua_State *L)
{
    AVFilterContext *ctx = get_ctx(L);
    const int out_id = luaL_checkinteger(L, 1);
    int ret = 0;

    if (ctx->nb_outputs > out_id && out_id >= 0) {
        ret = ctx->outputs[out_id]->frame_count;
    }

    lua_pushinteger(L, ret);

    return 1;
}

static int script_log(lua_State *L)
{
    AVFilterContext *ctx = get_ctx(L);
    int msgl = luaL_checkinteger(L, 1);
    int last = lua_gettop(L);

    lua_getglobal(L, "tostring"); // args... tostring
    for (int i = 2; i <= last; i++) {
        lua_pushvalue(L, -1); // args... tostring tostring
        lua_pushvalue(L, i); // args... tostring tostring args[i]
        lua_call(L, 1, 1); // args... tostring str
        const char *s = lua_tostring(L, -1);
        if (s == NULL)
            return luaL_error(L, "Invalid argument");
        av_log(ctx, msgl, "%s%s", s, i > 0 ? " " : "");
        lua_pop(L, 1);  // args... tostring
    }
    av_log(ctx, msgl, "\n");

    return 0;
}

#define FN_ENTRY(name) {#name, script_ ## name}
struct fn_entry {
    const char *name;
    int (*fn)(lua_State *L);
};

static const struct fn_entry main_fns[] = {
    FN_ENTRY(log),
    FN_ENTRY(frame_count),
    FN_ENTRY(filter),
    {0}
};

static void push_module_table(lua_State *L, const char *module)
{
    av_assert0(L);

    lua_getglobal(L, "package"); // package
    lua_getfield(L, -1, "loaded"); // package loaded
    lua_remove(L, -2); // loaded
    lua_getfield(L, -1, module); // loaded module
    if (lua_isnil(L, -1)) {
        lua_pop(L, 1); // loaded
        lua_newtable(L); // loaded module
        lua_pushvalue(L, -1); // loaded module module
        lua_setfield(L, -3, module); // loaded module
    }
    lua_remove(L, -2); // module
}

static void register_package_fns(lua_State *L, const char *module,
                                 const struct fn_entry *e)
{
    push_module_table(L, module);
    for (int n = 0; e[n].name; n++) {
        lua_pushcclosure(L, e[n].fn, 0);
        lua_setfield(L, -2, e[n].name);
    }
    lua_pop(L, 1);
}

static void add_functions(AVFilterContext *ctx)
{
    LuaScriptContext *s = ctx->priv;
    lua_State *L = s->L;

    register_package_fns(L, "lavfi", main_fns);
    push_module_table(L, "lavfi");

    lua_pop(L, 1);
}

static int run_lua(lua_State *L)
{
    AVFilterContext *ctx = lua_touserdata(L, -1);
    LuaScriptContext *s = ctx->priv;
    int ret = 0;

    lua_pop(L, 1);

    luaL_openlibs(L);

    lua_pushlightuserdata(L, ctx);
    lua_setfield(L, LUA_REGISTRYINDEX, "ctx");

    add_functions(ctx);

    push_module_table(L, "lavfi");

    lua_pushvalue(L, -1);
    lua_setglobal(L, "lavfi");

    lua_pushstring(L, s->script);
    lua_setfield(L, -2, "script_name");

    lua_pushcfunction(L, error_handler);
    lua_pushcfunction(L, load_script);
    if (lua_pcall(L, 0, 0, -2)) {
        const char *e = lua_tostring(L, -1);
        av_log(ctx, AV_LOG_ERROR, "%s\n", e ? e : "(unknown)");
        ret = AVERROR(EINVAL);
    }

    return ret;
}

static int run_script(AVFilterContext *ctx)
{
    LuaScriptContext *s = ctx->priv;
    const char *dump;
    int ret, i;

    if (s->L)
        lua_close(s->L);

    s->L = luaL_newstate();
    if (!s->L)
        return AVERROR(ENOMEM);

    avfilter_graph_free(&s->graph);
    memset(s->fctx, 0, sizeof(s->fctx));
    s->nbf = 0;

    s->graph = avfilter_graph_alloc();
    if (!s->graph)
        return AVERROR(ENOMEM);

    if (lua_cpcall(s->L, run_lua, ctx)) {
        const char *err = "unknown error";
        if (lua_type(s->L, -1) == LUA_TSTRING)
            err = lua_tostring(s->L, -1);
        av_log(ctx, AV_LOG_ERROR, "%s\n", err);
        return AVERROR(EINVAL);
    }

    link_filters(ctx);

    for (i = 0; i < ctx->nb_inputs; i++) {
        ret = link_src(ctx->inputs[i]);
        if (ret < 0)
            return ret;
    }

    for (i = 0; i < ctx->nb_outputs; i++) {
        ret = link_sink(ctx->outputs[i]);
        if (ret < 0)
            return ret;
    }

    ret = avfilter_graph_config(s->graph, ctx);
    if (ret < 0) {
        av_log(ctx, AV_LOG_ERROR, "Error configuring the filter graph\n");
        return ret;
    }

    // avfilter_config_links(ctx);

    dump = avfilter_graph_dump(s->graph, NULL);
    av_log(ctx, AV_LOG_DEBUG, "\n%s\n", dump);
    av_freep(&dump);

    return 0;
}

static int luascript_request_frame(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    unsigned out_id = FF_OUTLINK_IDX(outlink);
    LuaScriptContext *s = ctx->priv;
    int i, ret;

    for (i = 0; i < ctx->nb_inputs; i++) {
        av_log(ctx, AV_LOG_DEBUG, "in%d: %"PRId64" reqs:%"PRId64"\n", i, s->stats.nb_ins[i], s->stats.nb_reqs[i]);
    }

    for (i = 0; i < ctx->nb_outputs; i++) {
        av_log(ctx, AV_LOG_DEBUG, "out%d: %"PRId64"\n", i, s->stats.nb_outs[i]);
    }

    if (s->graph_configured) {
        if (outlink->frame_count - s->prev_oframe_count >= 50) {
            s->graph_configured = 1;
            s->prev_oframe_count = outlink->frame_count;
        }
    }
    if (!s->graph_configured) {
        ret = run_script(ctx);
        if (ret < 0)
            return ret;
        s->graph_configured = 1;
    }

    av_log(ctx, AV_LOG_DEBUG, "request_frame on outlink %d\n", out_id);

    for (i = 0; i < ctx->nb_inputs; i++) {
        if (ctx->inputs[i]->status) {
            s->eof[i] = 1;
        } else if (s->pending[i] == 0) {
            av_log(ctx, AV_LOG_DEBUG, "request_frame from inlink %d\n", i);
            ret = ff_request_frame(ctx->inputs[i]);
            if (ret < 0)
                return ret;
            s->stats.nb_reqs[i]++;
        }
    }

    for (i = 0; i < ctx->nb_inputs; i++) {
        if (!s->eof[i])
            break;
    }
    if (i == ctx->nb_inputs) {
        return AVERROR_EOF;
    }

    return luascript_push_frames(ctx, out_id);
}

static int luascript_config_input_props(AVFilterLink *inlink)
{
    return link_src(inlink);
}

static int luascript_config_output_props(AVFilterLink *outlink)
{
    AVFilterContext *ctx = outlink->src;
    unsigned out_id = FF_OUTLINK_IDX(outlink);
    LuaScriptContext *s  = ctx->priv;
    AVFilterLink *sinklink;
    int ret;

    ret = run_script(ctx);
    if (ret < 0)
        return ret;

    sinklink = s->sink[out_id]->inputs[0];
    outlink->time_base = sinklink->time_base;
    switch (outlink->type) {
    case AVMEDIA_TYPE_AUDIO:
        outlink->channel_layout = sinklink->channel_layout;
        outlink->channels       = sinklink->channels;
        outlink->sample_rate    = sinklink->sample_rate;
        break;
    case AVMEDIA_TYPE_VIDEO:
        outlink->w                   = sinklink->w;
        outlink->h                   = sinklink->h;
        outlink->sample_aspect_ratio = sinklink->sample_aspect_ratio;
        outlink->frame_rate          = sinklink->frame_rate;
        break;
    }

    return 0;
}

static av_cold int luascript_init(AVFilterContext *ctx)
{
    LuaScriptContext *s = ctx->priv;
    lua_State *L = s->L;
    int ret = 0;
    int i, j;

    if (!s->script) {
        av_log(ctx, AV_LOG_ERROR, "No filename provided!\n");
        return AVERROR(EINVAL);
    }

    s->graph = avfilter_graph_alloc();
    if (!s->graph)
        return AVERROR(ENOMEM);

    s->L = L = luaL_newstate();
    if (!L)
        return AVERROR(ENOMEM);

    if (lua_cpcall(L, run_lua, ctx)) {
        const char *err = "unknown error";
        if (lua_type(L, -1) == LUA_TSTRING)
            err = lua_tostring(L, -1);
        av_log(ctx, AV_LOG_ERROR, "%s\n", err);
        ret = AVERROR(EINVAL);
        goto fail;
    }

    link_filters(ctx);

    for (i = 0; i < s->nbf; i++) {
        FilterContext *fctx = &s->fctx[i];

        for (j = 0; j < fctx->nb_inputs; j++) {
            AVFilterContext *f = fctx->f;
            AVFilterPad pad = { 0 };

            if (fctx->linked_in[j])
                continue;

            s->fsrcid[ctx->nb_inputs] = i;
            s->fsrcpad[ctx->nb_inputs] = j;

            av_log(ctx, AV_LOG_DEBUG, "input: %s:%s\n", f->name, fctx->in[j]);
            pad.type          = avfilter_pad_get_type(f->input_pads, j);
            pad.name          = av_strdup(avfilter_pad_get_name(f->input_pads, j));
            if (!pad.name)
                return AVERROR(ENOMEM);
            pad.filter_frame     = luascript_filter_frame;
            pad.config_props     = luascript_config_input_props;
            ff_insert_inpad(ctx, ctx->nb_inputs, &pad);
        }
    }

    for (i = 0; i < s->nbf; i++) {
        FilterContext *fctx = &s->fctx[i];

        for (j = 0; j < fctx->nb_outputs; j++) {
            AVFilterContext *f = fctx->f;
            AVFilterPad pad = { 0 };

            if (fctx->linked_out[j])
                continue;

            s->fsinkid[ctx->nb_outputs] = i;
            s->fsinkpad[ctx->nb_outputs] = j;

            av_log(ctx, AV_LOG_DEBUG, "output: %s:%s\n", f->name, fctx->out[j]);
            pad.type          = avfilter_pad_get_type(f->output_pads, j);
            pad.name          = av_strdup(fctx->out[j]);
            if (!pad.name)
                return AVERROR(ENOMEM);
            pad.config_props  = luascript_config_output_props;
            pad.request_frame = luascript_request_frame;
            ff_insert_outpad(ctx, ctx->nb_outputs, &pad);
        }
    }

fail:
    return ret;
}

static av_cold void luascript_uninit(AVFilterContext *ctx)
{
    LuaScriptContext *s = ctx->priv;
    int i;

    if (s->L)
        lua_close(s->L);
    avfilter_graph_free(&s->graph);

    for (i = 0; i < ctx->nb_inputs; i++) {
        av_freep(&ctx->input_pads[i].name);
    }

    for (i = 0; i < ctx->nb_outputs; i++) {
        av_freep(&ctx->output_pads[i].name);
    }
}

static int luascript_query_formats(AVFilterContext *ctx)
{
    int i, ret;

    for (i = 0; i < ctx->nb_outputs; i++) {
        AVFilterFormats *formats, *rates = NULL;
        AVFilterChannelLayouts *layouts = NULL;
        AVFilterLink *outlink = ctx->outputs[i];

        formats = ff_all_formats(outlink->type);
        if ((ret = ff_formats_ref(formats,
                                  &outlink->in_formats)) < 0)
            return ret;
        switch (outlink->type) {
        case AVMEDIA_TYPE_AUDIO:
            rates = ff_all_samplerates();
            if ((ret = ff_formats_ref(rates,
                                      &outlink->in_samplerates)) < 0)
                return ret;
            layouts = ff_all_channel_layouts();
            if ((ret = ff_channel_layouts_ref(layouts,
                                              &outlink->in_channel_layouts)) < 0)
                return ret;
            break;
        }
    }

    for (i = 0; i < ctx->nb_inputs; i++) {
        AVFilterFormats *formats, *rates = NULL;
        AVFilterChannelLayouts *layouts = NULL;
        AVFilterLink *inlink = ctx->inputs[i];

        formats = ff_all_formats(inlink->type);
        if ((ret = ff_formats_ref(formats,
                                  &inlink->out_formats)) < 0)
            return ret;
        switch (inlink->type) {
        case AVMEDIA_TYPE_AUDIO:
            rates = ff_all_samplerates();
            if ((ret = ff_formats_ref(rates,
                                      &inlink->out_samplerates)) < 0)
                return ret;
            layouts = ff_all_channel_layouts();
            if ((ret = ff_channel_layouts_ref(layouts,
                                              &inlink->out_channel_layouts)) < 0)
                return ret;
            break;
        }
    }

    return 0;
}

AVFILTER_DEFINE_CLASS(luascript);

AVFilter ff_avsrc_luascript = {
    .name          = "luascript",
    .description   = NULL_IF_CONFIG_SMALL("Lavfi lua script."),
    .priv_size     = sizeof(LuaScriptContext),
    .priv_class    = &luascript_class,
    .init          = luascript_init,
    .uninit        = luascript_uninit,
    .query_formats = luascript_query_formats,
    .inputs        = NULL,
    .outputs       = NULL,
    .flags         = AVFILTER_FLAG_DYNAMIC_INPUTS | AVFILTER_FLAG_DYNAMIC_OUTPUTS,
};
