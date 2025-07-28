#include <stdint.h>
#include <stdlib.h>
#include <sys/epoll.h>

struct epoll_event *make_epoll_event_voidptr_c(uint32_t events, void *ptr);

struct epoll_event *make_epoll_event_int_c(uint32_t events, int fd);

struct epoll_event *make_epoll_event_uint32_c(uint32_t events, uint32_t u32);

struct epoll_event *make_epoll_event_uint64_c(uint32_t events, uint64_t u64);

void free_epoll_event_c(struct epoll_event *ptr);
