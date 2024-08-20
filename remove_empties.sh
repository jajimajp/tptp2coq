#!/bin/bash

usage() {
  echo "Usage: $0 /path/to/directory"
  exit 1
}

if [ "$#" -eq 1 ]
then
  if [[ "$1" == -* ]]
  then
    usage
  fi
else
  usage
fi

find $1 -type f -empty -delete
find $1 -type d -empty -delete
