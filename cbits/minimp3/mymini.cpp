#include <cstdint>
#include <filesystem>
#include <fstream>
#include <ios>
#include <iostream>
#include <vector>

#define MINIMP3_IMPLEMENTATION
#define MINIMP3_ONLY_MP3
#define MINIMP3_NO_SIMD
#include "minimp3.h"

std::vector<std::uint8_t> read_file_data(const std::filesystem::path &path) {
  std::ifstream file(path, std::ios::binary | std::ios::ate);

  if (!file)
    return {};

  std::streamsize file_size = file.tellg();

  if (file_size < 0)
    return {};

  std::vector<std::uint8_t> buffer(file_size);

  file.seekg(0, std::ios::beg);

  if (!file.read(reinterpret_cast<char *>(buffer.data()), file_size))
    return {};

  return buffer;
}

void decode(mp3dec_t &mp3dec, mp3dec_frame_info_t &info,
            std::vector<uint8_t>&& mp3_data, std::vector<int16_t>&& pcm_data) {
  std::uint8_t *start = mp3_data.data();
  int length = mp3_data.size();
  int total_consumed = 0;
  while (length > 0) {
    int samples =
        mp3dec_decode_frame(&mp3dec, start, length, pcm_data.data(), &info);

    int consumed = info.frame_bytes;
    length -= consumed;
    start = start + consumed;
    total_consumed += consumed;

    if (samples > 0 && consumed > 0) {
    } else if (samples == 0 and consumed > 0) {
      std::cout << "Skipped ID3 or invalid data\n";
    } else if (samples == 0 && consumed == 0) {
      std::cout << "Insufficient data\n";
    }
  }

  std::cout << "Total consume == mp3 size: " << (total_consumed == mp3_data.size()) << "\n";
}

int main() {
  mp3dec_t mp3dec;
  mp3dec_init(&mp3dec);
  mp3dec_frame_info_t info;
  std::vector<int16_t> pcm(MINIMP3_MAX_SAMPLES_PER_FRAME);

  std::vector<uint8_t> mp3_data =
      read_file_data("/home/james/rats-in-ruin.mp3");

  decode(mp3dec, info, std::move(mp3_data), std::move(pcm));

  return 0;
}
