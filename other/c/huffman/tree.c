#include <stdlib.h>

#include "heap.h"
#include "tree.h"

// === PRIVATE FUNCTIONS ===

/// Create a new node with the given weight.
Node *node_create(int weight) {
  Node *node = malloc(sizeof(Node));
  node->isLeaf = false;
  node->weight = weight;
  node->left = NULL;
  node->right = NULL;

  return node;
}

/// Create a new leaf node with the given weight and associated character.
Node *leaf_create(int weight, char chr) {
  Node *leaf = node_create(weight);
  leaf->isLeaf = true;
  leaf->chr = chr;

  return leaf;
}

/// Destroy the given node and all of its children, if they exist.
void node_destroy(Node *node) {
  if (!node) {
    return;
  }

  if (!node->isLeaf) {
    node_destroy(node->left);
    node_destroy(node->right);
  }

  free(node);
}

/// Insert a map entry into the heap given by the data pointer.
void add_to_heap(MapEntry *entry, void *data) {
  Heap *heap = (Heap *)data;
  heap_insert(heap, leaf_create(entry->value, entry->key));
}

/// Build a map from characters to their corresponding codes.
void build_code_map(Node *root, Map *codes, int path) {
  if (!root) {
    return;
  }

  if (root->isLeaf) {
    map_set(codes, root->chr, path);
  } else {
    build_code_map(root->left, codes, path << 1);
    build_code_map(root->right, codes, (path << 1) + 1);
  }
}

// === PUBLIC FUNCTIONS ===

Tree *tree_create(Map *counts) {
  Tree *tree = malloc(sizeof(Tree));

  // Build the tree itself.
  Heap *heap = heap_create(counts->size);
  map_foreach(counts, add_to_heap, heap);

  while (heap->size > 1) {
    Node *left = heap_remove(heap);
    Node *right = heap_remove(heap);
    Node *parent = node_create(left->weight + right->weight);
    parent->left = left;
    parent->right = right;

    heap_insert(heap, parent);
  }

  tree->root = heap_remove(heap);
  heap_destroy(heap);

  // Build the code map.
  Map *code_map = map_create();
  // Note the hidden assumption here: a tree with only one (leaf) node will be
  // encoded by 0.
  build_code_map(tree->root, code_map, 0);
  tree->codes = code_map;

  return tree;
}

void tree_destroy(Tree *tree) {
  node_destroy(tree->root);
  tree->root = NULL;
  map_destroy(tree->codes);
  tree->codes = NULL;
  free(tree);
}
