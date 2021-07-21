//clang++ -o check_update check_update.cpp -I/Users/byul/code/supercollider/include/common -I/Users/byul/code/SuperCollider/include/plugin_interface/ -I/Users/byul/Code/supercollider/include/server -std=c++11
#include "SC_World.h"
#include "SC_WorldOptions.h"

int main(int argc, char *argv[]) {
  printf("World Size: %lu\n", sizeof(struct World));
  printf("WorldOptions Size: %lu\n", sizeof(struct WorldOptions));
  
  return 0;
}
