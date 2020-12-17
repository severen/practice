#include <iostream>
#include <cmath>

int fibonacci(int n) {
  const double phi = (1 + std::sqrt(5)) / 2;
  const double psi = 1 - phi;

  // Binet's Formula
  return (int)((std::pow(phi, n) - std::pow(psi, n)) / std::sqrt(5));
}

int main() {
  for (int i = 0; i < 10; i++) {
    std::cout << "Fibonacci number #" << i + 1 << ": " << fibonacci(i) << "\n";
  }

  return 0;
}
