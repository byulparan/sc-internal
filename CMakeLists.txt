cmake_minimum_required(VERSION 3.11)

project(scsynth-add)


set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(SC3_SRC "/Users/byul/Dropbox/code/tools/SuperCollider")
set(CFFI_EXPORT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/lib")

include(~/Dropbox/code/tools/link/AbletonLinkConfig.cmake)



set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17 -I/usr/local/include -I/opt/homebrew/include")
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

endif()





add_library(scsynth-add SHARED scsynth_add.cpp link.cpp)

target_include_directories(scsynth-add PUBLIC
  "${SC3_SRC}/include/server"
  "${SC3_SRC}/include/plugin_interface"
  "${SC3_SRC}/include/common"
)

target_link_libraries(scsynth-add ${LIBSCSYNTH} Ableton::Link)





