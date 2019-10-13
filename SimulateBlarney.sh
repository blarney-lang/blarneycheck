#!/bin/bash

FILEDIR=${1?Error: no dir given}
FILENAME=${2?Error: no file given}

cd $FILEDIR
blc $FILENAME > /dev/null
echo "Compiled! $FILEDIR$FILENAME"

for O in $(ls *.out); do
  TEST=$(basename $O .out)
  ./$TEST
  cd $TEST-Verilog
  make -s &> /dev/null
  echo "Executed $TEST with result of:"
  ./top | head -n -1
done

cd ..
rm -rf *.o *.hi Sorter Sorter-Verilog
