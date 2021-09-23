#!/bin/bash

echo -n "Delete input [y/n]?"
read input

if [ $input = y ]
then
  rm -rf ../data/input/clm/*/
fi

echo -n "Delete cases [y/n]?"
read input2

if [ $input2 = y ]
then
  rm -rf ../data/cases/*/
fi

echo -n "Delete output [y/n]?"
read input3

if [ $input3 = y ]
then
  rm -rf ../data/output/*/
fi
