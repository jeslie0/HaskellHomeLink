CMAKE_MINIMUM_REQUIRED(VERSION 3.11)

PROJECT(audio LANGUAGES C CXX VERSION 0.0.1)


add_executable(${PROJECT_NAME} main.cpp)

find_package(PkgConfig)
pkg_check_modules(ALSA
  REQUIRED
  IMPORTED_TARGET
  alsa
)

target_link_libraries(${PROJECT_NAME} PkgConfig::ALSA -lm)
target_include_directories (${PROJECT_NAME} PUBLIC ${ALSA_INCLUDE_DIRS})
# message("${ALSA_INCLUDE_DIRS}")
# target_compile_options (${PROJECT_NAME} PUBLIC ${ALSA_CFLAGS_DIRS})
