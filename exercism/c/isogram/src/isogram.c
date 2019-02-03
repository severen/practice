#include "isogram.h"

bool is_isogram(const char word[]) {
  if (!word) {
    return false;
  }

  char found[26] = {0};
  for (size_t i = 0; word[i] != '\0'; i++) {
    char c = word[i];

    if (!isalpha(c)) {
      continue;
    }

    int i = tolower(c) - 'a';

    if (found[i]) {
      return false;
    } else {
      found[i] = true;
    }
  }

  return true;
}
