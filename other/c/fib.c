#include <stdio.h>

#include "common.h"

// DESCRIPTION
//   fib - find the nth fibonacci number
// SYNOPSIS
//   fib n
// BUILDING
//   $ meson build
//   $ meson compile -C build

// Note that this function uses 0-based indexing for the Fibonacci numbers.
unsigned long fib(long n) {
  unsigned long a = 0;
  unsigned long b = 1;

  for (long i = 0; i < n; ++i) {
    unsigned long next = a + b;

    a = b;
    b = next;
  }

  return a;
}

int main(int argc, char *argv[]) {
  if (argc == 1) {
    fprintf(stderr, "Please specify a number.\n");
    return 1;
  }

  long n = parse_long(argv[1]);
  if (n < 1) {
    fprintf(stderr, "The input number must be 1 or greater.\n");
    return 1;
  }
  printf("Fibonacci number #%lu is %lu.\n", n, fib(n - 1));

  return 0;
}
