#include "epoll_hs.h"

#include <stdint.h>
#include <stdlib.h>
#include <sys/epoll.h>

struct epoll_event *make_epoll_event_voidptr_c(uint32_t events, void *ptr) {
  struct epoll_event *event_ptr = malloc(sizeof(struct epoll_event));
  event_ptr->events = events;
  event_ptr->data.ptr = ptr;
  return event_ptr;
}

struct epoll_event *make_epoll_event_int_c(uint32_t events, int fd) {
  struct epoll_event *event_ptr = malloc(sizeof(struct epoll_event));
  event_ptr->events = events;
  event_ptr->data.fd = fd;
  return event_ptr;
}

struct epoll_event *make_epoll_event_uint32_c(uint32_t events, uint32_t u32) {
  struct epoll_event *event_ptr = malloc(sizeof(struct epoll_event));
  event_ptr->events = events;
  event_ptr->data.u32 = u32;
  return event_ptr;
}

struct epoll_event *make_epoll_event_uint64_c(uint32_t events, uint64_t u64) {
  struct epoll_event *event_ptr = malloc(sizeof(struct epoll_event));
  event_ptr->events = events;
  event_ptr->data.u64 = u64;
  return event_ptr;
}

void free_epoll_event_c(struct epoll_event *ptr) {
  free(ptr);
  ptr = NULL;
}
