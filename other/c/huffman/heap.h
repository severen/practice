#include "tree.h"

#ifndef HEAP_H
#define HEAP_H

/// A binary heap of Huffman tree nodes.
typedef struct Heap {
  // NOTE: We use int instead of size_t here in order to simplify the
  //       arithmetic in the heapSink and heapSwim functions.
  /// The size of the heap.
  int size;
  /// The maximum capacity of the heap.
  int capacity;

  /// The array of nodes that constitutes the heap.
  Node **nodes;
} Heap;

/// Create a new heap with the given capacity.
Heap *heap_create(int capacity);
/// Free the given heap.
void heap_destroy(Heap *heap);

/// Insert a node into the heap.
void heap_insert(Heap *heap, Node *node);
/// Remove and return the node that is on top of the heap.
Node *heap_remove(Heap *heap);

#endif /* HEAP_H */
