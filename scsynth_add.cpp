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
  void sbcl_send_signal();
  void sbcl_quit_signal();
  int sbcl_printf(const char*, va_list);
  void communicate_init(char* _buffer);
  void sbcl_reply_func(struct ReplyAddress*, char*, int);
  struct World* make_world(struct WorldOptions*);
}


char* lisp_buffer;

<<<<<<< HEAD
pthread_mutex_t mutex;
pthread_cond_t  cond_var1;
pthread_cond_t  cond_var2;
=======
std::recursive_mutex mutex;
std::condition_variable_any cond;
>>>>>>> master

int sbcl_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void communicate_init(char* _buffer) {
  lisp_buffer = _buffer;
  pthread_mutex_init(&mutex, nullptr);
  pthread_cond_init(&cond_var1, nullptr);
  pthread_cond_init(&cond_var2, nullptr);
}

void sbcl_lock() {
  mutex.lock();
}

void sbcl_unlock() {
  mutex.unlock();
}

void sbcl_wait_signal() {
<<<<<<< HEAD
  pthread_cond_wait(&cond_var1, &mutex);
}

void sbcl_send_signal() {
  pthread_cond_signal(&cond_var2);
}

void sbcl_quit_signal() {
  pthread_cond_signal(&cond_var1);
=======
  std::unique_lock<std::recursive_mutex> lk(mutex);
  mutex.unlock();
  cond.wait(lk);
  mutex.lock();
>>>>>>> master
}

void sbcl_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  std::unique_lock<std::recursive_mutex> lk(mutex);
  memcpy(lisp_buffer, inBuf, inSize);
  sbcl_message_size = inSize;
<<<<<<< HEAD
  pthread_cond_signal(&cond_var1);
  pthread_cond_wait(&cond_var2, &mutex);
  pthread_mutex_unlock(&mutex);
}
=======
  cond.notify_one();
}        
>>>>>>> master

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


