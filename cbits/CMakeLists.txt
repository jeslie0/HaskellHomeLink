CMAKE_MINIMUM_REQUIRED(VERSION 3.11)

PROJECT(audio LANGUAGES C VERSION 0.0.1)


add_executable(${PROJECT_NAME} main.c)


find_package(PkgConfig)
pkg_check_modules(PIPEWIRE REQUIRED libpipewire-0.3)


target_link_libraries(${PROJECT_NAME} ${PIPEWIRE_LIBRARIES} m)
target_include_directories (${PROJECT_NAME} PUBLIC ${PIPEWIRE_INCLUDE_DIRS})
target_compile_options (${PROJECT_NAME} PUBLIC ${PIPEWIRE_CFLAGS_DIRS})
