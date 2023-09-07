#include <inttypes.h>
#include <stdio.h>

// Command to compile to .o file:
//   $ cc -c runtime.c

void print_int(int64_t x) {
  printf("%ld\n", x);
}
