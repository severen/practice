#!/usr/bin/env bash

set -o errexit -o nounset

if [[ $# -ne 1 ]]; then
  echo "Usage: ./error_handling <greetee>"
  exit 1
fi

echo "Hello, $1"
