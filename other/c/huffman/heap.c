#include <stdlib.h>
#include <assert.h>

#include "heap.h"

// === PRIVATE FUNCTIONS ===

/// Get the index of the parent of the node at position i in the heap.
static inline int parent_index(int i) {
  return (i - 1) / 2;
}

/// Get the index of the leftmost child of the node at position i in the heap.
static inline int child_index(int i) {
  return (2 * i) + 1;
}

/// Check whether the node at position i in the heap has a greater weight than
/// the node at position j.
static inline bool is_larger(Heap *heap, int i, int j) {
  return heap->nodes[i]->weight > heap->nodes[j]->weight;
}

/// Swap the node at position i in the heap with the node at position j.
static inline void heap_swap(Heap *heap, int i, int j) {
  Node *tmp = heap->nodes[i];
  heap->nodes[i] = heap->nodes[j];
  heap->nodes[j] = tmp;
}

/// Restore the heap property of the given heap by "swimming" the node at
/// position i up the heap.
void heap_swim(Heap *heap, int i) {
  while (i > 0 && is_larger(heap, parent_index(i), i)) {
    heap_swap(heap, parent_index(i), i);
    i = parent_index(i);
  }
}

/// Restore the heap property of the given heap by "sinking" the node at
/// position i down the heap.
void heap_sink(Heap *heap, int i) {
  while (child_index(i) < heap->size - 1) {
    int j = child_index(i);

    // Move to the rightmost child if it is smaller.
    if (j < heap->size - 2 && is_larger(heap, j, j + 1)) {
      j++;
    }

    // Stop sinking this node if its smallest child is greater than or equal to
    // it.
    if (!is_larger(heap, i, j)) {
      break;
    }

    // Otherwise move the current node down a level by swapping it with its
    // child.
    heap_swap(heap, i, j);
    i = j;
  }
}

// === PUBLIC FUNCTIONS ===

Heap *heap_create(int capacity) {
  Heap *heap = malloc(sizeof(Heap));
  heap->size = 0;
  heap->capacity = capacity;
  heap->nodes = calloc((size_t)heap->capacity, sizeof(Node*));

  return heap;
}

void heap_destroy(Heap *heap) {
  if (!heap) {
    return;
  }

  // NOTE: The nodes themselves are not freed because the tree (see tree.h),
  //       not the heap, has ownership of them.
  free(heap->nodes);
  heap->nodes = NULL;
  free(heap);
}

void heap_insert(Heap *heap, Node *node) {
  assert(heap->size <= heap->capacity);

  heap->nodes[heap->size++] = node;
  heap_swim(heap, heap->size - 1);
}

Node *heap_remove(Heap *heap) {
  if (heap->size == 0) {
    // NULL is OK as an error value because there is no correct situation where
    // there should be a null reference in the heap.
    return NULL;
  }

  Node *min = heap->nodes[0];

  heap_swap(heap, 0, heap->size - 1);
  heap->nodes[heap->size - 1] = NULL;
  heap->size--;
  heap_sink(heap, 0);

  return min;
}
