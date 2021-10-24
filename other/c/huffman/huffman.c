#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "map.h"
#include "tree.h"

/// Create a map from the characters in the given file to their corresponding
/// frequencies.
Map *count_chars(FILE *file) {
  Map *counts = map_create();

  int res;
  while ((res = fgetc(file)) != EOF) {
    unsigned char chr = (unsigned char)res;
    if (map_contains(counts, chr)) {
      int new_count = map_get(counts, chr) + 1;
      map_set(counts, chr, new_count);
    } else {
      map_set(counts, chr, 1);
    }
  }

  return counts;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    exit(1);
  }

  FILE *file = fopen(argv[1], "r");
  if (file == NULL) {
    fprintf(stderr, "Error opening %s: %s.\n", argv[1], strerror(errno));
    exit(1);
  }

  Map *counts = count_chars(file);
  fclose(file);
  Tree *tree = tree_create(counts);
  map_destroy(counts);
  tree_destroy(tree);
}
