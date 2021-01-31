#include <iostream>
#include <string>
#include <stdexcept>

// DESCRIPTION
//   fib - find the nth fibonacci number
// SYNOPSIS
//   fib n
// BUILDING
//   $ meson build
//   $ meson compile -C build

// Note that this function uses 0-based indexing for Fibonacci numbers.
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
  if (argc < 2) {
    std::cerr << "Please specify a number.\n";
    return 1;
  }

  long n;
  try {
    n = std::stol(argv[1], nullptr);
  } catch (const std::invalid_argument& e) {
    std::cerr << "The input " << argv[1] << " is not a number.\n";
    return 1;
  } catch (const std::out_of_range& e) {
    std::cerr << "The input " << argv[1] << " is out of the range for a long int.\n";
    return 1;
  }

  if (n < 1) {
    std::cerr << "The input must be 1 or greater.\n";
    return 1;
  }
  std::cout << "Fibonacci number #" << n << " is " << fib(n - 1) << ".\n";

  return 0;
}
