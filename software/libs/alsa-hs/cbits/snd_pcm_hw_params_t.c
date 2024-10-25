#include <alsa/asoundlib.h>
#include "snd_pcm_hw_params_t.h"

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

int snd_pcm_hw_params_allocate(snd_pcm_hw_params_t *params)
{
    return ;
}
