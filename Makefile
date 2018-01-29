CXX=clang++
CXXFLAGS= -I$(SC3_SRC)/include/server -I$(SC3_SRC)/include/common/ -I$(SC3_SRC)/include/plugin_interface -std=c++14

LIBSCSYNTH_ADD = libscsynth_add.dylib

all : $(LIBSCSYNTH_ADD)

$(LIBSCSYNTH_ADD) : scsynth_add.cpp
ifndef SC3_SRC
	$(error "You must set environment variable SC3_SRC") 
endif 
	$(CXX) -shared -o $@ scsynth_add.cpp -L./ -lscsynth.1.0.0 $(CXXFLAGS)

clean :
	rm -f $(LIBSCSYNTH_ADD)
