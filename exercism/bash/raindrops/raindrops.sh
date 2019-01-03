#!/usr/bin/env bash

set -o errexit -o nounset

hasfactor=false

if [[ $(($1 % 3)) -eq 0 ]]; then
  printf "Pling"
  hasfactor=true
fi

if [[ $(($1 % 5)) -eq 0 ]]; then
  printf "Plang"
  hasfactor=true
fi

if [[ $(($1 % 7)) -eq 0 ]]; then
  printf "Plong"
  hasfactor=true
fi

if [[ $hasfactor == false ]]; then
  echo "$1"
fi
