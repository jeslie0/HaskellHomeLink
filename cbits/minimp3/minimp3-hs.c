#include "minimp3-hs.h"

mp3dec_t* new_mp3dec_t()
{
    return malloc(sizeof(mp3dec_t));
}

void free_mp3dec_t(mp3dec_t* ptr)
{
    free(ptr);
}

mp3dec_frame_info_t* new_mp3dec_frame_info_t()
{
    return malloc(sizeof(mp3dec_frame_info_t));
}

void free_mp3dec_frame_info_t(mp3dec_frame_info_t* ptr)
{
    free(ptr);
}

int get_frame_bytes_unsafe(mp3dec_frame_info_t* info)
{
    return info->frame_bytes;
}

int get_channels_unsafe(mp3dec_frame_info_t* info)
{
    return info->channels;
}

int get_hz_unsafe(mp3dec_frame_info_t* info)
{
    return info->hz;
}

int get_layer_unsafe(mp3dec_frame_info_t* info)
{
    return info->layer;
}

int get_bitrate_kbps_unsafe(mp3dec_frame_info_t* info)
{
    return info->bitrate_kbps;
}
