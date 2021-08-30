cmake_minimum_required(VERSION 3.0)

project(scsynth_add)

set(SC3_SRC "/Users/byul/code/SuperCollider")
set(CFFI_EXPORT_DIR "/Users/byul/lib")


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CFFI_EXPORT_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})

if(APPLE)
  set(CMAKE_INSTALL_NAME_DIR @rpath)
  set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)
  
  find_library(LIBSCSYNTH
    NAMES libscsynth.1.0.0.dylib
    HINTS ${CFFI_EXPORT_DIR}
    REQUIRED)
  
elseif(UNIX)
  find_library(LIBSCSYNTH
    NAMES libscsynth.so
    HINTS /usr/local/lib
    REQUIRED)

endif(APPLE)



add_library(scsynth_add SHARED scsynth_add.cpp)

target_include_directories(scsynth_add PUBLIC
  "${SC3_SRC}/include/server"
  "${SC3_SRC}/include/plugin_interface"
  "${SC3_SRC}/include/common")

target_link_libraries(scsynth_add ${LIBSCSYNTH})

