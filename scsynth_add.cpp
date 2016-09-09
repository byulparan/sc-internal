#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "SC_WorldOptions.h"

int sc_fd = -1;
static int _fd[2];

extern "C" {
  int sbcl_printf(const char*, va_list);
  void communicate_init();
  void communicate_dealloc();
  void sbcl_reply_func(struct ReplyAddress*, char*, int);
  struct World* make_world(struct WorldOptions*);
}

int sbcl_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void communicate_init() {
  int result = pipe(_fd);
  sc_fd = _fd[0];
}

void communicate_dealloc() {
  sc_fd = -1;
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  int size[1] = {inSize};
  write(_fd[1], size, 4);
  write(_fd[1], inBuf, inSize);
}

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


