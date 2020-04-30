#!/bin/bash
# Run from the work directory in BlarneyCheck

FILE=${1?Error: no file given}
FILENAME=$(basename "$FILE")
FILEDIR=$(dirname "$FILE")

if ! [[ -d "/opt/ghc/bin" || -x "$(command -v ghc)" ]]; then
  echo "Installing ghc files in /opt/ghc/bin..."
  sudo add-apt-repository -y ppa:hvr/ghc
  sudo apt-get update
  sudo apt-get install -y ghc-8.6.5
fi

pkgs='verilator'
if ! dpkg -s $pkgs >/dev/null 2>&1; then
  sudo apt-get install $pkgs
fi

echo "Running: $FILE"

PATH="/opt/ghc/bin:$PATH" BLARNEY_ROOT="blarney" "blarney/Scripts/blc" "$FILE"

cd $FILEDIR
EXECUTABLE=$(basename $FILENAME .hs)
if [ -f "./$EXECUTABLE" ]; then
  ./$EXECUTABLE
  cd Out-Verilog
  make -s #&> /dev/null
  echo "Executed $EXECUTABLE with result of:"
  mkdir -p ../Results/$EXECUTABLE
  (time ./top | head -n -1) 2>&1 | tee "../Results/$EXECUTABLE/output_$(date +"%Y_%d_%m_%H_%M_%S").txt"

  cd "$FILEDIR"
fi

rm -rf *.o *.hi "$FILEDIR/$EXECUTABLE" Out-Verilog
