#include <stdlib.h>

#include "map.h"

// === PRIVATE FUNCTIONS ===

// NOTE: This number is chosen because it is a prime number and ensures that
//       each ASCII character with a value below 127 (i.e. typical English
//       letters, punctuation, and numbers) will fit in a unique bucket.
/// The size of the underlying array used to store map entries.
const int NUM_BUCKETS = 127;

/// Create a new entry key/value pair.
MapEntry *map_entry_create(unsigned char key, int value) {
  MapEntry *entry = malloc(sizeof(MapEntry));
  entry->key = key;
  entry->value = value;
  entry->next = NULL;

  return entry;
}

/// Calculate the index in the map for a given key.
static inline int map_index(unsigned char key) {
  // Technically, the key could be negative because whether or not char is
  // signed or unsigned is implementation defined. However, this should never
  // be the case in reality since we are only ever dealing with chars that
  // contain (ASCII/UTF-8) code units, which are positive.
  return key % NUM_BUCKETS;
}

// === PUBLIC FUNCTIONS ===

Map *map_create(void) {
  Map *map = malloc(sizeof(Map));
  map->size = 0;
  map->entries = calloc((size_t)NUM_BUCKETS, sizeof(MapEntry*));

  return map;
}

void map_destroy(Map *map) {
  if (!map) {
    return;
  }

  for (int i = 0; i < NUM_BUCKETS; ++i) {
    MapEntry *current_entry = map->entries[i];

    while (current_entry) {
      MapEntry *next_entry = current_entry->next;
      free(current_entry);

      current_entry = next_entry;
    }
  }

  free(map->entries);
  map->entries = NULL;
  free(map);
}

void map_set(Map *map, unsigned char key, int value) {
  int i = map_index(key);

  if (!map->entries[i]) {
    map->entries[i] = map_entry_create(key, value);
    map->size++;
  } else {
    MapEntry *entry = map->entries[i];
    do {
      if (entry->key == key) {
        entry->value = value;
        return;
      }

      entry = entry->next;
    } while (entry->next);

    entry->next = map_entry_create(key, value);
    map->size++;
  }
}

int map_get(Map *map, unsigned char key) {
  int i = map_index(key);

  if (map->entries[i]) {
    MapEntry *entry = map->entries[i];
    while (entry->next && entry->key != key) {
      entry = entry->next;
    }

    return entry->value;
  }

  // The caller can distinguish the error value of -1 from a key value of -1 by
  // using the map_contains function.
  return -1;
}

bool map_contains(Map *map, unsigned char key) {
  int i = map_index(key);

  MapEntry *entry = map->entries[i];
  while (entry) {
    if (entry->key == key) {
      return true;
    }

    entry = entry->next;
  }

  return false;
}

void map_foreach(Map *map, void (*f)(MapEntry *, void *), void *f_data) {
  for (int i = 0; i < NUM_BUCKETS; ++i) {
    MapEntry *entry = map->entries[i];
    if (!entry) {
      continue;
    }

    while (entry) {
      f(entry, f_data);
      entry = entry->next;
    }
  }
}
