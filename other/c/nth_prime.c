#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>

long isqrt(long n) {
  double result = sqrt(n);
  if (isnan(result) && errno == EDOM) {
    return -1;
  }

  // The truncation done by this cast is equivalent to floor(sqrt(n)) because
  // sqrt(n) is always positive.
  return (long)result;
}

bool is_prime(long n) {
  if (n < 2) { return false; }

  for (long m = 2; m <= isqrt(n); m++) {
    if (n % m == 0) {
      return false;
    }
  }

  return true;
}

long nth_prime(long n) {
  if (n < 1) { exit(EXIT_FAILURE); }

  long count = 0;
  for (long i = 2; ; i++) {
    if (is_prime(i) && count++ == n) {
      return i;
    }
  }
}

int main(int argc, char *argv[]) {
  // TODO: Check for this at compile time (or just don't bother at all).
  assert((math_errhandling & MATH_ERRNO) != 0);

  if (argc == 1) {
    fprintf(stderr, "Please specify a number.\n");
    return 1;
  }

  long n = strtol(argv[1], NULL, 10);
  if (n == 0 && errno == EINVAL) {
    fprintf(stderr, "The input %s is not a number.\n", argv[1]);
    return 1;
  } else if ((n == LONG_MIN || n == LONG_MAX) && errno == ERANGE) {
    fprintf(stderr, "The input %s is out of the range for a long int.\n", argv[1]);
    return 1;
  }

  if (n < 1) {
    fprintf(stderr, "The input number must be 1 or greater.\n");
    return 1;
  }
  printf("Prime #%li is %li\n", n, nth_prime(n));

  return 0;
}
