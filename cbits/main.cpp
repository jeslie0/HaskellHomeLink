#include <iostream>
#include <cmath>
#include <array>
#include <alsa/asoundlib.h>

const int SAMPLE_RATE = 44100;  // Sample rate in Hz
const int FREQUENCY = 440;               // Frequency of the sine wave in Hz (A4 note)
const int DURATION = 5;                  // Duration in seconds
const int AMPLITUDE = 32000;             // Amplitude of the sine wave (max 32767 for 16-bit audio)
const int CHANNELS = 1;                  // Number of audio channels
const int BUFFER_SIZE = SAMPLE_RATE;            // Buffer size (number of frames)

void generate_sine_wave(int16_t *buffer, int num_samples, int frequency, int sample_rate, int amplitude, int start_sample) {
    for (int i = 0; i < num_samples; ++i) {
        buffer[i] = static_cast<int16_t>(amplitude * sin((2.0 * M_PI * frequency * (start_sample + i)) / sample_rate));
    }
}

int main() {
    // Initialize ALSA
    snd_pcm_t *pcm_handle;
    snd_pcm_hw_params_t *params;
    int dir;

    // Open PCM device for playback
    int pcm = snd_pcm_open(&pcm_handle, "default", SND_PCM_STREAM_PLAYBACK, 0);
    if (pcm < 0) {
        std::cerr << "ERROR: Can't open PCM device. " << snd_strerror(pcm) << std::endl;
        return -1;
    }

    // Allocate a hardware parameters object
    snd_pcm_hw_params_malloc(&params);

    // Fill it in with default values
    snd_pcm_hw_params_any(pcm_handle, params);
    snd_pcm_hw_params_set_buffer_size(pcm_handle, 1);
    snd_pcm_hw_params_set_period_size(pcm_handle, params, 1);

    // Set the desired hardware parameters
    snd_pcm_hw_params_set_access(pcm_handle, params, SND_PCM_ACCESS_RW_INTERLEAVED);
    snd_pcm_hw_params_set_format(pcm_handle, params, SND_PCM_FORMAT_S16_LE);
    snd_pcm_hw_params_set_channels(pcm_handle, params, CHANNELS);
    unsigned int sample_rate = SAMPLE_RATE; // Use a non-const variable
    pcm = snd_pcm_hw_params_set_rate_near(pcm_handle, params, &sample_rate, &dir);
    if (pcm < 0) {
        std::cerr << "ERROR: Can't set sample rate. " << snd_strerror(pcm) << std::endl;
        return -1;
    }

    // Write the parameters to the driver
    pcm = snd_pcm_hw_params(pcm_handle, params);
    if (pcm < 0) {
        std::cerr << "ERROR: Can't set hardware parameters. " << snd_strerror(pcm) << std::endl;
        return -1;
    }

    // Calculate the number of total samples
    int total_samples = SAMPLE_RATE * DURATION;

    // Create double buffers
    std::array<int16_t, BUFFER_SIZE> buffer1;

    // Initialize the first buffer
    generate_sine_wave(buffer1.data(), BUFFER_SIZE, FREQUENCY, SAMPLE_RATE, AMPLITUDE, 0);

    int current_sample = 0;

    // Lambda function to generate buffer
    auto generate_buffer = [&](int16_t *buffer, int start_sample) {
        generate_sine_wave(buffer, BUFFER_SIZE, FREQUENCY, SAMPLE_RATE, AMPLITUDE, start_sample);
    };

    // Start playback
    while (true) {
        std::cout << "LOOP\n";
        int frames = snd_pcm_writei(pcm_handle, buffer1.data(), BUFFER_SIZE);
        if (frames < 0) frames = snd_pcm_recover(pcm_handle, frames, 0);
        if (frames < 0) {
            std::cerr << "ERROR: Can't write to PCM device. " << snd_strerror(frames) << std::endl;
            break;
        }

        current_sample += BUFFER_SIZE;

        // Generate the next buffer in a separate thread
        // std::thread generation_thread(generate_buffer, buffer2.data(), current_sample);
        // generation_thread.join();
        generate_buffer(buffer1.data(), current_sample);

        // Swap buffers

    }

    // Drain the audio
    snd_pcm_drain(pcm_handle);
    snd_pcm_close(pcm_handle);
    snd_pcm_hw_params_free(params);

    std::cout << "Finished playing sine wave." << std::endl;
    return 0;
}
