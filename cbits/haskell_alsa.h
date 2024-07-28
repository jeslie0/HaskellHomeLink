#ifndef HASKELL_ALSA_H
#define HASKELL_ALSA_H

#include <alsa/asoundlib.h>
#include <alsa/pcm.h>

// * Handle
snd_pcm_t *snd_pcm_t_new(char* name, snd_pcm_stream_t stream, int mode);

void snd_pcm_t_free(snd_pcm_t *pcm_handle);

int open_pcm_for_stream(void** handle_ptr_ptr, char* name);

void with_handle_c(void (*f) (snd_pcm_t*));


// * Parameters
snd_pcm_hw_params_t* snd_pcm_hw_params_t_new();

void snd_pcm_hw_params_t_free(snd_pcm_hw_params_t *params);

/* int snd_pcm_hw_params_malloc_c(void** parms_ptr_ptr); */

#endif // HASKELL_SND_PCM_T_H
