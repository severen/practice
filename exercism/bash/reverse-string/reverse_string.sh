#!/usr/bin/env bash

set -o errexit -o nounset

echo "$@" | rev
