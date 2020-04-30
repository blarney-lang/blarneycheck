#!/bin/bash

cd /home/jonas/Part_II_Project/blarneycheck/work
examples="ActoraStack BRAMStack CPU FirstHot Sorter Sums Sums_Parallel"

for i in {1..16}
do
   for ex in $examples
   do
      /home/jonas/Part_II_Project/blarneycheck/Temp/SimulateBlarney.sh "BuggySuite/$ex.hs" "--eval"
   done
done

cd BuggySuite/Results/Evaluation
for ex in $examples
do
   cd "$ex"
   printf "\n$ex:\n"
   grep -rohP '(?<=user\t).*' ./
   cd ..
done
