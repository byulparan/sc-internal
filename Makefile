
CXX=clang++
CXXFLAGS= -I/Users/byul/code/SC3/sources/supercollider/include/server -I/Users/byul/code/SC3/sources/supercollider/include/common/

# TARGET = libshm_interface.dylib
TARGET = libscsynth_add.dylib
OBJ = scsynth_add.o

all : $(TARGET)

$(TARGET) : $(OBJ)
	$(CXX) -shared -o $(TARGET) $(OBJ) -L./ -lscsynth.1.0.0
clean :
	rm $(TARGET) $(OBJ)
