#include <iostream>
#include <string>
#include <semaphore>
#include <boost/lockfree/queue.hpp>

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
};


std::counting_semaphore gSemaphore{0};
boost::lockfree::queue<Message> gQueue(2048);


int sc_lisp_printf(const char *fmt, va_list ap) {
  vprintf(fmt, ap);
  fflush(stdout);
  return 0;
}

void sc_lisp_reply_thread(char* lisp_buf, void (*cl_reply_callback)()) {
  Message dummy;
  while(gQueue.pop(dummy));
  
  gReplyThreadRunning = true;

  while(true) {
    gSemaphore.acquire();
    
    if (!gReplyThreadRunning) break;

    Message m;
    if(gQueue.pop(m)) {
      memcpy(lisp_buf, m.mMessage, m.mSize);
      cl_reply_callback();
    }
  }
  
}


void sc_lisp_reply_quit() {

  gReplyThreadRunning = false;
  gSemaphore.release();
}

void sc_lisp_reply_func (struct ReplyAddress *inReplyAddr, char* inBuf, int inSize) {
  if (inSize >= 2048) {
    std::cout<<"message size: "<<inSize<<" is too long. this reply message will ignore."<<std::endl;
    return;
  }
  
  Message m;
  memcpy(m.mMessage, inBuf, inSize);
  m.mSize = inSize;
  
  gQueue.push(m);
  gSemaphore.release();
}        

struct World* make_world(struct WorldOptions* opt) {
  return World_New(opt);
}


