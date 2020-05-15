#!/bin/bash

cd /home/jonas/Part_II_Project/blarneycheck/work/
wc -l {Check/*.hs,BuggySuite/{ActoraStack,BRAMStack,CPU,FirstHot,MemAddr,RandomCheck,Sums,Sums_Parallel}.hs,CheckFPGA_*/Src/*.hs}
