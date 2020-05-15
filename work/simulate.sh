#!/bin/bash

# Parse filename to execute
FILE=${1?Error: no file given}
FILENAME=$(basename "$FILE")
FILEDIR=$(dirname "$FILE")
GHC="/opt/ghc/bin"
BLC="blarney/Scripts/blc"

# Check log flags
if [[ $* == *--log* ]]; then
if [[ $* == *--eval* ]]; then
  OUTDIR="Evaluation"
else
  OUTDIR="Simulation"
fi
fi

# Check for ghc
if ! [[ -d $GHC || -x "$(command -v ghc)" ]]; then
  echo "GHC not found! Please install using the following commands (sudo):"
  echo "add-apt-repository -y ppa:hvr/ghc && apt-get update && apt-get install -y ghc-8.6.5"
  exit 1
fi

# Check for additional packages
pkgs='verilator'
if ! dpkg -s $pkgs >/dev/null 2>&1; then
  echo "$pkgs not found! Please install before simulation. Eg:"
  echo "apt-get install $pkgs"
  exit 1
fi

# Check for BLARNEYCHECK_ROOT variable
if [ -z "$BLARNEYCHECK_ROOT" ]; then
  export BLARNEYCHECK_ROOT="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
  echo "BLARNEYCHECK_ROOT not set, defaulting to $BLARNEYCHECK_ROOT"
fi

# Check that blc is modified
if ! grep -q "BLARNEYCHECK_ROOT" "$BLARNEYCHECK_ROOT/$BLC"; then
  echo "Modified Blarney blc script to include BLARNEYCHECK_ROOT"
  sed -i -e 's/INC="\$BLARNEY_ROOT\/Haskell"/INC="\$BLARNEY_ROOT\/Haskell:\$BLARNEYCHECK_ROOT\/Haskell:\$BLARNEYCHECK_ROOT\/BuggySuite"/g' "$BLARNEYCHECK_ROOT/$BLC"
fi

# Compile file
echo "Compiling: $FILE"
PATH="$GHC:$PATH" BLARNEY_ROOT="${BLARNEY_ROOT:-$BLARNEYCHECK_ROOT/blarney}" eval "$BLARNEYCHECK_ROOT/$BLC" $FILE

cd $FILEDIR
# Check that compilation succeeded
EXECUTABLE=$(basename $FILENAME .hs)
if [ ! -f "./$EXECUTABLE" ]; then
  echo ""
  echo "File failed to compile!"
  exit 1
fi

# Execute file to generate verilog
./$EXECUTABLE
VERILOG_DIR=(./*-Verilog/)
if [ -z "$VERILOG_DIR" ]; then
  echo ""
  echo "No verilog was generated!"
  echo "Make sure that the main Haskell function is:"
  echo "main = writeVerilogTop <testBench> \"top\" \"<Name>-Verilog/\""
  exit 1
fi

cd $VERILOG_DIR
# Compile verilog to executable
make -s
echo "Executing $EXECUTABLE with result of:"
mkdir -p ../Results/$OUTDIR/$EXECUTABLE
if [ -z "$OUTDIR" ]; then
  (time ./top | head -n -1) 2>&1
else
  (time ./top | head -n -1) 2>&1 | tee "../Results/$OUTDIR/$EXECUTABLE/output_$(date +"%Y_%d_%m_%H_%M_%S").txt"
fi

cd ..
rm -f *.o *.hi "./$EXECUTABLE"
rm -rf $VERILOG_DIR
