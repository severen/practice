#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int fibonacci(int n) {
  if (n < 0) { exit(EXIT_FAILURE); }
  
  double phi = (1 + sqrt(5)) / 2;
  double psi = 1 - phi;
  
  // Binet's Formula
  return (int)((pow(phi, n) - pow(psi, n)) / sqrt(5));
}

int main(void) {
  for (int i = 0; i < 10; i++) {
    printf("Fibonacci number #%d: %d\n", i + 1, fibonacci(i));
  }

  return 0;
}
