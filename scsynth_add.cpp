#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>
#include "SC_WorldOptions.h"

int size = 0;
char* buf = NULL;

pthread_mutex_t mutex;
pthread_cond_t read_condition_var;
pthread_cond_t write_condition_var;

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
  pthread_mutex_init(&mutex, NULL);
  pthread_cond_init(&read_condition_var, NULL);
  pthread_cond_init(&write_condition_var, NULL);
}

void communicate_dealloc() {
  pthread_mutex_destroy(&mutex);
  pthread_cond_destroy(&read_condition_var);
  pthread_cond_destroy(&write_condition_var);
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  pthread_mutex_lock(&mutex);
  size = inSize;
  buf = inBuf;
  pthread_cond_signal(&read_condition_var);
  pthread_cond_wait(&write_condition_var, &mutex);
  size = 0;
  pthread_mutex_unlock(&mutex);
}

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}
