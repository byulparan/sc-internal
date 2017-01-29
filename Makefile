
CXX=clang++
CXXFLAGS= -I/Users/byul/code/supercollider/include/server -I/Users/byul/code/supercollider/include/common/ -I/Users/byul/code/supercollider/include/plugin_interface 

# TARGET = libshm_interface.dylib
LIBSCSYNTH_ADD = libscsynth_add.dylib
CHECK_UPDATE = check_update
OBJ = scsynth_add.o

all : $(LIBSCSYNTH_ADD) $(CHECK_UPDATE)

$(LIBSCSYNTH_ADD) : $(OBJ)
	$(CXX) -shared -o $@ $(OBJ) -L./ -lscsynth.1.0.0
	install_name_tool -change libscsynth.1.0.0.dylib `pwd`/libscsynth.1.0.0.dylib $@

$(CHECK_UPDATE) :check_update.cpp
	$(CXX) -o $@ check_update.cpp $(CXXFLAGS)

clean :
	rm $(LIBSCSYNTH_ADD) $(OBJ) $(CHECK_UPDATE)
