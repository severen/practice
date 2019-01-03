#!/usr/bin/env bash

# Script that checks if the provided date is a leap year.
#
# Only input that is a single non-negative integer is accepted (read: a year).

set -o errexit -o nounset

# Exit if more or less than one argument has been provided, or if the input is
# not a positive integer.
if [[ $# -ne 1 || ! $1 =~ ^[=+]?[0-9]+$ ]]; then
  echo 'Usage: leap.sh <year>'
  exit 1
fi

year="$1"
if [[ $((year % 4)) -eq 0 && ($((year % 100)) -ne 0 || $((year % 400)) -eq 0) ]]; then
  echo 'true'
else
  echo 'false'
fi
