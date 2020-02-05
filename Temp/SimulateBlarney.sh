#!/bin/bash

FILEDIR=${1?Error: no dir given}
FILENAME=${2?Error: no file given}


pwd
echo "Filedir: $FILEDIR"
echo "Filename: $FILENAME"

if ! [[ -d "/opt/ghc/bin" || -x "$(command -v ghc)" ]]; then
  # Take action if $DIR exists. #
  echo "Installing ghc files in /opt/ghc/bin..."
  sudo add-apt-repository -y ppa:hvr/ghc
  sudo apt-get update
  sudo apt-get install -y ghc-8.6.5
fi

pkgs='verilator'
if ! dpkg -s $pkgs >/dev/null 2>&1; then
  sudo apt-get install $pkgs
fi

File=~/.bashrc
PathAddGhc='[ -d "/opt/ghc/bin" ] && export PATH="/opt/ghc/bin:$PATH"'
if ! grep -q "$PathAddGhc" "$File"; then
  [ -d "/opt/ghc/bin" ] && echo "$PathAddGhc" >> "$File"
fi


PATH="/opt/ghc/bin:$PATH" BLARNEY_ROOT=blarney blarney/Scripts/blc "$FILEDIR/$FILENAME" #> /dev/null
echo "Compiled! $FILEDIR/$FILENAME"

cd $FILEDIR
TEST=$(basename $FILENAME .hs)
./$TEST
cd Out-Verilog
make -s #&> /dev/null
echo "Executed $TEST with result of:"
./top | head -n -1

cd ..

rm -rf *.o *.hi "$TEST" Out-Verilog
