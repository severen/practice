#!/usr/bin/env bash

set -o errexit -o nounset

score=0
chars=$(echo "$1" | grep -o . | tr '[:upper:]' '[:lower:]')

for char in $chars; do
  case $char in
    [aeioulnrst])
      ((score+=1))
      ;;

    [dg])
      ((score+=2))
      ;;

    [bcmp])
      ((score+=3))
      ;;

    [fhvwy])
      ((score+=4))
      ;;

    k)
      ((score+=5))
      ;;

    [jx])
      ((score+=8))
      ;;

    [qz])
      ((score+=10))
      ;;
  esac
done

echo $score
