#!/bin/bash

cd /home/jonas/Part_II_Project/blackcheck/work
examples="MemAddr OneHot Sorter Stack Sums Sums_Fast"

for i in {1..16}
do
   for ex in $examples
   do
      /home/jonas/Part_II_Project/blackcheck/Temp/SimulateBlarney.sh BuggySuite "$ex".hs
   done
done

cd BuggySuite/Results
for ex in $examples
do
   cd "$ex"
   printf "\n$ex:\n"
   grep -rohP '(?<=user\t).*' ./
   cd ..
done
