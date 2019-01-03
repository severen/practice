#!/usr/bin/env bash

set -o errexit -o nounset

# The calculations could have been done in pure Bash, but I personally believe
# using bc is more elegant *and* readable.

if [[ "$1" == "total" ]]; then
  # Derived from S = a * (1 - r^n)/(1 - r).
  echo "(1 - 2^64)/(1 - 2)" | bc
  exit 0
elif [[  "$1" -le 0 || "$1" -gt 64 ]]; then
  echo "Error: invalid input"
  exit 1
fi

# Derived from aₙ = ar^(n - 1).
echo "2^($1 - 1)" | bc
