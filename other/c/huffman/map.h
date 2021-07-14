#include <stdbool.h>

#ifndef MAP_H
#define MAP_H

/// An entry in a map.
typedef struct MapEntry {
  /// The key for this entry.
  char key;
  /// The value for this entry.
  int value;

  /// The next entry in the list.
  struct MapEntry *next;
} MapEntry;

/// A map from characters to integers.
typedef struct Map {
  /// The size of the map.
  int size;

  /// The array of linked lists of map entries that constitutes the map.
  MapEntry **entries;
} Map;

/// Create a new map.
Map *map_create(void);
/// Free the given map.
void map_destroy(Map *map);

/// Set the value for a given key.
void map_set(Map *map, char key, int value);
/// Get the value for a given key.
int map_get(Map *map, char key);

/// Check if the map contains a given key.
bool map_contains(Map *map, char key);

/// Execute the given function on each entry in the map.
void map_foreach(Map *map, void (*f)(MapEntry *, void *), void *f_data);

#endif /* MAP_H */
