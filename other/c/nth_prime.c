#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <errno.h>

#include "common.h"

// DESCRIPTION
//   nth_prime - find the nth prime
// SYNOPSIS
//   nth_prime n
// BUILDING
//   $ meson build
//   $ meson compile -C build

#if ((math_errhandling & MATH_ERRNO) == 0)
  #error "This program requires support for floating point error handling via errno."
#endif

int isqrt(int n) {
  double result = sqrt(n);
  if (isnan(result) && errno == EDOM) {
    return -1;
  }

  // The truncation done by this cast is equivalent to floor(sqrt(n)) because
  // sqrt(n) is always positive.
  return (int)result;
}

bool is_prime(int n) {
  if (n < 2) { return false; }

  for (int m = 2; m <= isqrt(n); ++m) {
    if (n % m == 0) {
      return false;
    }
  }

  return true;
}

// Note that this function uses 0-based indexing for the primes.
int nth_prime(int n) {
  int count = 0;
  for (int i = 2; ; ++i) {
    if (is_prime(i) && count++ == n) {
      return i;
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Please specify a number.\n");
    return 1;
  }

  int n = parse_int(argv[1]);
  if (n < 1) {
    fprintf(stderr, "The input number must be 1 or greater.\n");
    return 1;
  }
  printf("Prime #%i is %i.\n", n, nth_prime(n - 1));

  return 0;
}
