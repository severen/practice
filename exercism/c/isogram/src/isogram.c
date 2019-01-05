#include "isogram.h"

int cmp(const void* a, const void* b) {
  return *(const char*)a < *(const char*)b;
}

bool is_isogram(const char phrase[]) {
  if (!phrase) {
    return false;
  }

  // Copy string into writable memory for qsort.
  char* sorted = malloc(strlen(phrase) + 1);
  strncpy(sorted, phrase, strlen(phrase));

  qsort(sorted, strlen(sorted), sizeof(char), cmp);

  for (int i = 0; sorted[i]; i++) {
    if (tolower(sorted[i]) == tolower(sorted[i + 1]) && isalpha(sorted[i])) {
      free(sorted);
      return false;
    }
  }

  free(sorted);
  return true;
}
