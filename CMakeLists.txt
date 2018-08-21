cmake_minimum_required(VERSION 2.6)

project(scsynth_add)

set(SC3_SRC "$ENV{SC3_SRC}")
if(SC3_SRC) 
	message("SC3_SRC set to ${SC3_SRC}")
else()
    message(FATAL_ERROR "Please define the SC3_SRC environment variable to point to SuperCollider source folder.")
endif()


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

if(APPLE)
  set(CMAKE_INSTALL_NAME_DIR @rpath)
  set(CMAKE_BUILD_WITH_INSTALL_NAME_DIR ON)
  
  find_library(LIBSCSYNTH
    NAMES libscsynth.1.0.0.dylib
    HINTS ${CMAKE_CURRENT_SOURCE_DIR}
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

