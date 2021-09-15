#!/bin/bash

echo -n "Delete input [y/n]?"
read input

if [ $input = y ]
then
  rm -rf ../data/input/*/
fi

echo -n "Delete cases [y/n]?"
read input2

if [ $input2 = y ]
then
  rm -rf ../data/cases/*/
fi

# Delete cases
# cd ../data
