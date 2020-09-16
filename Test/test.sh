#! /usr/bin/env bash

# Run regression tests

BLARNEY_EXAMPLES=(
  ActoraStack
  BRAMStack
  CPU
  FirstHot
  MemAddr
  RandomCheck
  Sorter
  Sums
  Sums_Parallel
)

BC_ROOT="$(dirname "$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )")"
if [ -z "$BLARNEY_ROOT" ]; then
  export BLARNEY_ROOT="$BC_ROOT/blarney"
fi

BLARNEY_EXAMPLES="${BLARNEY_EXAMPLES[@]}" BLARNEY_TESTING_ROOT=$BC_ROOT "$BLARNEY_ROOT/Test/test.sh" $@
