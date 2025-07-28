#ifndef HASKELL_ALSA_H
#define HASKELL_ALSA_H

#include <alsa/asoundlib.h>
#include <alsa/pcm.h>

// * Handle
snd_pcm_t *snd_pcm_t_new();

void snd_pcm_t_free_unused(snd_pcm_t *pcm_handle);

int void_snd_pcm_open(void **handle_ptr, char *name, snd_pcm_stream_t stream,
                      int mode);

// * Parameters

snd_pcm_hw_params_t *snd_pcm_hw_params_t_new();

void snd_pcm_hw_params_t_free_unused(snd_pcm_hw_params_t *params);

int void_snd_pcm_hw_params_malloc(void **params);

#endif // HASKELL_SND_PCM_T_H
