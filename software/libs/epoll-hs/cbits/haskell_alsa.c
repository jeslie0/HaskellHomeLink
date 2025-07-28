#include <alsa/asoundlib.h>
#include <alsa/error.h>

// * Sound device constructor and destructor. Create a sound device
// ptr. We don't open it here though.
snd_pcm_t *snd_pcm_t_new() {
  snd_pcm_t *ptr;
  return ptr;
}

void snd_pcm_t_free_unused(snd_pcm_t *pcm_handle) {}

int void_snd_pcm_open(void **handle_ptr_ptr, char *name,
                      snd_pcm_stream_t stream, int mode) {
  return snd_pcm_open((snd_pcm_t **)handle_ptr_ptr, name, stream, mode);
}

// Make Parameters

snd_pcm_hw_params_t *snd_pcm_hw_params_t_new() {
  snd_pcm_hw_params_t *ptr;
  return ptr;
}

void snd_pcm_hw_params_t_free_unused(snd_pcm_hw_params_t *params) {}

void void_snd_pcm_hw_params_malloc(void **params) {
  snd_pcm_hw_params_malloc((snd_pcm_hw_params_t **)params);
}
