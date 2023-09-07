#include <inttypes.h>
#include <stdio.h>

// Command to compile to .o file:
//   $ cc -c runtime.c

void print_int(int64_t x) {
  printf("%ld\n", x);
}

int64_t read_int() {
  int64_t result;
  scanf("%ld", &result);
  return result;
}
