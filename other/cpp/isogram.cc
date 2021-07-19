#include <iostream>
#include <string>
#include <span>
#include <array>

// DESCRIPTION
//   isogram - determine whether a word is an isogram
// SYNOPSIS
//   isogram word
// BUILDING
//   $ meson build
//   $ ninja -C build

// The size of the English alphabet.
const int ALPHABET_SIZE = 26;

bool is_isogram(const std::string& word) {
  std::array<bool, ALPHABET_SIZE> found{};

  for (auto c : word) {
    if (isalpha(c) == 0) {
      continue;
    }

    size_t i = (size_t)tolower(c) - 'a';
    if (!found[i]) {
      found[i] = true;
    } else {
      return false;
    }
  }

  return true;
}

int main(int argc, char *argv[]) {
  auto args = std::span(argv, size_t(argc));

  if (args.size() < 2) {
    std::cerr << "Please provide a word.\n";
    return 1;
  }

  auto word = std::string(args[1]);
  if (is_isogram(word)) {
    std::cout << "The word '" << word << "' is an isogram.\n";
  } else {
    std::cout << "The word '" << word << "' is not an isogram.\n";
  }

  return 0;
}
