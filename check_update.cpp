
#include "SC_World.h"
#include "SC_WorldOptions.h"

int main(int argc, char *argv[]) {
  printf("World Size: %lu\n", sizeof(struct World));
  printf("WorldOptions Size: %lu\n", sizeof(struct WorldOptions));
  
  return 0;
}
