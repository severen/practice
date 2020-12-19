#include <iostream>
#include <string>
#include <array>

bool is_isogram(const std::string& word) {
  std::array<char, 26> found = { 0 };

  for (auto c : word) {
    if (!isalpha(c)) {
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
  if (argc < 2) {
    std::cerr << "Please provide a word.\n";
    return 1;
  }

  std::string word = argv[1];
  if (is_isogram(word)) {
    std::cout << "The given word is an isogram.\n";
  } else {
    std::cout << "The given word is not an isogram.\n";
  }

  return 0;
}
