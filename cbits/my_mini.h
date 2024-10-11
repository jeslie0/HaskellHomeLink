#ifndef MY_AMINIMP3_H
#define MY_AMINIMP3_H

#define MINIMP3_IMPLEMENTATION
#define MINIMP3_ONLY_MP3
#define MINIMP3_NO_SIMD
#include "minimp3.h"

mp3dec_t* new_mp3dec_t();
void free_mp3dec_t(mp3dec_t* ptr);

mp3dec_frame_info_t* new_mp3dec_frame_info_t();
void free_mp3dec_frame_info_t(mp3dec_frame_info_t* ptr);

int get_frame_bytes_unsafe(mp3dec_frame_info_t* info);
int get_channels_unsafe(mp3dec_frame_info_t* info);
int get_hz_unsafe(mp3dec_frame_info_t* info);
int get_layer_unsafe(mp3dec_frame_info_t* info);
int get_bitrate_kbps_unsafe(mp3dec_frame_info_t* info);

#endif
