#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

int isqrt(int n) {
  return (int)floor(sqrt(n));
}

bool is_prime(int n) {
  if (n < 2) { return false; }

  for (int m = 2; m <= isqrt(n); m++) {
    if (n % m == 0) {
      return false;
    }
  }

  return true;
}

int nth_prime(int n) {
  int counter = 0;

  if (n == 0) { exit(EXIT_FAILURE); }

  for (int i = 2; ; i++) {
    if (is_prime(i)) {
      counter++;
      
      if (counter == n) {
        return i;
      }
    }
  }
}

int main(void) {  
  for (int i = 1; i <= 10; i++) {
    printf("Prime #%d: %d\n", i, nth_prime(i));
  }

  return 0;
}
