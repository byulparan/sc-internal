#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <mutex>
#include <condition_variable>
#include "SC_WorldOptions.h"


extern "C" {
  int sbcl_message_size;
  void sbcl_lock();
  void sbcl_unlock();
  void sbcl_wait_signal();
  int sbcl_printf(const char*, va_list);
  void communicate_init(char* _buffer);
  void sbcl_reply_func(struct ReplyAddress*, char*, int);
  struct World* make_world(struct WorldOptions*);
}


char* lisp_buffer;

std::recursive_mutex mutex;
std::condition_variable_any cond;

int sbcl_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void communicate_init(char* _buffer) {
  lisp_buffer = _buffer;
}

void sbcl_lock() {
  mutex.lock();
}

void sbcl_unlock() {
  mutex.unlock();
}

void sbcl_wait_signal() {
  std::unique_lock<std::recursive_mutex> lk(mutex);
  mutex.unlock();
  cond.wait(lk);
  mutex.lock();
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  std::unique_lock<std::recursive_mutex> lk(mutex);
  memcpy(lisp_buffer, inBuf, inSize);
  sbcl_message_size = inSize;
  cond.notify_one();
}        

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


