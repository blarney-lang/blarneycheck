#!/bin/bash

FILEDIR=${1?Error: no dir given}
FILENAME=${2?Error: no file given}


pwd
echo "Filedir: $FILEDIR"
echo "Filename: $FILENAME"
blc "$FILEDIR/$FILENAME" #> /dev/null
echo "Compiled! $FILEDIR/$FILENAME"

cd $FILEDIR
TEST=$(basename $FILENAME .hs)
./$TEST
cd Out-Verilog
make -s #&> /dev/null
echo "Executed $TEST with result of:"
./top | head -n -1

cd ..

rm -rf *.o *.hi Out-Verilog
find . -type f  ! -name "*.*"  -delete
