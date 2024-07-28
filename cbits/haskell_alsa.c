#include <alsa/asoundlib.h>

snd_pcm_t *snd_pcm_t_new(char* name, snd_pcm_stream_t stream, int mode) {
  snd_pcm_t *ptr;
  snd_pcm_open(&ptr, name, stream, mode);
  return ptr;
}

void snd_pcm_t_free(snd_pcm_t *pcm_handle) {
  snd_pcm_drain(pcm_handle);
  snd_pcm_close(pcm_handle);
}

int open_pcm_for_stream(void** handle_ptr_ptr, char* name)
{
    return snd_pcm_open((snd_pcm_t**)handle_ptr_ptr, name, SND_PCM_STREAM_PLAYBACK, 0);
}

snd_pcm_hw_params_t* snd_pcm_hw_params_t_new()
{
    snd_pcm_hw_params_t* params;
    snd_pcm_hw_params_malloc(&params);
    return params;
}

void snd_pcm_hw_params_t_free(snd_pcm_hw_params_t *params)
{
    snd_pcm_hw_params_free(params);
}



void with_handle_c(void (*f) (snd_pcm_t*))
{
    snd_pcm_t* pcm_handle;
    f(pcm_handle);
}
