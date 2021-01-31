#include <iostream>
#include <string>
#include <stdexcept>
#include <cmath>
#include <cerrno>

// DESCRIPTION
//   nth_prime - find the nth prime
// SYNOPSIS
//   nth_prime n
// BUILDING
//   $ meson build
//   $ ninja -C build

#if ((math_errhandling & MATH_ERRNO) == 0)
  #error "This program requires support for floating point error handling via errno."
#endif

long isqrt(long n) {
  double result = std::sqrt(n);
  if (std::isnan(result) && errno == EDOM) {
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
  long count = 0;
  for (long i = 2; ; i++) {
    if (is_prime(i) && count++ == n) {
      return i;
    }
  }
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
    std::cerr << "The input number must be 1 or greater.\n";
    return 1;
  }
  std::cout << "Prime #" << n << " is " << nth_prime(n) << ".\n";

  return 0;
}
