#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
// #include <mutex>
// #include <condition_variable>
#include <pthread.h>
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

pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  cond_var  = PTHREAD_COND_INITIALIZER;

int sbcl_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void communicate_init(char* _buffer) {
  lisp_buffer = _buffer;
}

void sbcl_lock() {
  pthread_mutex_lock(&mutex);
}

void sbcl_unlock() {
  pthread_mutex_unlock(&mutex);
}

void sbcl_wait_signal() {
  pthread_cond_wait(&cond_var, &mutex);
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  pthread_mutex_lock(&mutex);
  memcpy(lisp_buffer, inBuf, inSize);
  sbcl_message_size = inSize;
  pthread_cond_signal(&cond_var);
  pthread_mutex_unlock(&mutex);
}

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


