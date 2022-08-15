#include <stdbool.h>

#include "map.h"

#ifndef TREE_H
#define TREE_H

/// A weighted node in a Huffman tree that is either an interior node with a
/// left and right child, or a leaf node with an associated character.
typedef struct Node {
  /// Whether this node is a leaf or not.
  bool is_leaf;

  /// The weight of this node.
  int weight;
  union {
    // Leaf node
    /// The character that corresponds to this leaf node.
    unsigned char chr;

    // Interior node
    struct {
      /// The left child of this interior node.
      struct Node *left;
      /// The right child of this interior node.
      struct Node *right;
    };
  };
} Node;

/// A Huffman tree.
typedef struct Tree {
  /// The root node of the Huffman tree.
  Node *root;
  /// A map from characters to their corresponding codes.
  Map *codes;
} Tree;

/// Create a Huffman tree from the given map of character counts.
Tree *tree_create(Map *counts);
/// Free the given Huffman tree.
void tree_destroy(Tree *tree);

#endif /* TREE_H */
