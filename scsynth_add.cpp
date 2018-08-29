#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <iostream>
#include <mutex>
#include <condition_variable>
#include "SC_WorldOptions.h"


extern "C" {
  int sbcl_message_size;
  void sbcl_lock();
  void sbcl_unlock();
  void sbcl_wait_signal();
  void sbcl_send_signal();
  void sbcl_quit_signal();
  int sbcl_printf(const char*, va_list);
  void communicate_init(char* _buffer);
  void sbcl_reply_func(struct ReplyAddress*, char*, int);
  struct World* make_world(struct WorldOptions*);
}

char* lisp_buffer;

std::recursive_mutex mutex;
std::condition_variable_any cond_var1;
std::condition_variable_any cond_var2;

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
  cond_var1.wait(lk);
  mutex.lock();
}

void sbcl_send_signal() {
  cond_var2.notify_one();
}

void sbcl_quit_signal() {
  cond_var1.notify_one();
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  std::unique_lock<std::recursive_mutex> lk(mutex);
  memset(lisp_buffer, 0, 2048);
  if (inSize > 2048) {
    std::cout<<"in reply message size too long: "<<inSize<<" > 2048."<<std::endl;
    std::cout<<"message cut until 2048."<<std::endl;
    fflush(stdout);
    inSize = 2048;
  }
  memcpy(lisp_buffer, inBuf, inSize);
  sbcl_message_size = inSize;
  cond_var1.notify_one();
  cond_var2.wait(lk);
}        

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


