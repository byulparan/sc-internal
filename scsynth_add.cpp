#include <iostream>
#include <string>
#include <queue>
#include <mutex>
#include <condition_variable>

#include "SC_WorldOptions.h"

extern "C" {
  bool gReplyThreadRunning;
  int sc_lisp_printf(const char*, va_list);
  void sc_lisp_reply_thread(char* _buffer, void (*cl_reply_callback)());
  void sc_lisp_reply_quit();
  void sc_lisp_reply_func(struct ReplyAddress*, char*, int);
  struct World* make_world(struct WorldOptions*);
}

struct Message {
  char mMessage[2048];
  int mSize;
  Message(int size, char* message) {
    mSize = size;
    memcpy(mMessage, message, size);
  }
};

static std::queue<Message> gMessageQueue;
std::recursive_mutex mutex;
std::condition_variable_any condition_var;


int sc_lisp_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void sc_lisp_reply_thread(char* lisp_buf, void (*cl_reply_callback)()) {
  while(!gMessageQueue.empty()) gMessageQueue.pop();
  gReplyThreadRunning = true;
  mutex.lock();
  while(true) {
    condition_var.wait(mutex);
										     
    if (!gReplyThreadRunning) break;
										     
    while(!gMessageQueue.empty()) {
      Message& m = gMessageQueue.front();
      memcpy(lisp_buf, m.mMessage, m.mSize);
      cl_reply_callback();
      gMessageQueue.pop();
    }
  }
  mutex.unlock();
}


void sc_lisp_reply_quit() {
  mutex.lock();
  gReplyThreadRunning = false;
  condition_var.notify_one();
  mutex.unlock();
}

void sc_lisp_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  if (inSize >= 2048) {
    std::cout<<"message size: "<<inSize<<" is too long. this reply message will ignore."<<std::endl;
    return;
  }
  mutex.lock();
  Message m(inSize, inBuf);
  gMessageQueue.push(m);
  condition_var.notify_one();
  mutex.unlock();
}        

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


