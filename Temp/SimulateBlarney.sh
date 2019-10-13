#!/bin/bash

FILEDIR=${1?Error: no dir given}
FILENAME=${2?Error: no file given}

cd $FILEDIR
blc $FILENAME > /dev/null
echo "Compiled! $FILEDIR$FILENAME"

TEST=$(basename $FILENAME .hs)
./$TEST
cd Out-Verilog
make -s &> /dev/null
echo "Executed $TEST with result of:"
./top | head -n -1

cd ..
rm -rf *.o *.hi $TEST Out-Verilog
