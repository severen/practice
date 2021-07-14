#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <math.h>
#include <limits.h>
#include <errno.h>

#include "common.h"

const int BASE_10 = 10;

int strtoi(const char *restrict str, char **restrict endptr, int base) {
  intmax_t n = strtoimax(str, endptr, base);
  if (n < INT_MIN) {
    errno = ERANGE;
    return INT_MIN;
  }
  if (n > INT_MAX) {
    errno = ERANGE;
    return INT_MAX;
  }

  return (int)n;
}

int parse_int(const char str[]) {
  int n = strtoi(str, NULL, BASE_10);
  if (n == 0 && errno == EINVAL) {
    fprintf(stderr, "The input %s is not a number.\n", str);
    exit(EXIT_FAILURE);
  } else if ((n == INT_MIN || n == INT_MAX) && errno == ERANGE) {
    fprintf(stderr, "The input %s is out of the range for an int.\n", str);
    exit(EXIT_FAILURE);
  }

  return n;
}

long parse_long(const char str[]) {
  long n = strtol(str, NULL, BASE_10);
  if (n == 0 && errno == EINVAL) {
    fprintf(stderr, "The input %s is not a number.\n", str);
    exit(EXIT_FAILURE);
  } else if ((n == LONG_MIN || n == LONG_MAX) && errno == ERANGE) {
    fprintf(stderr, "The input %s is out of the range for a long int.\n", str);
    exit(EXIT_FAILURE);
  }

  return n;
}
