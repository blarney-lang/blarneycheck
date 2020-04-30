#!/bin/bash

sleep 8
for i in {1..15}
do
  cp BlockRAM.v "dse/dse1_SoCKitTop/dse1_SoCKitTop_$i"
  cp BlockRAMTrueDual.v "dse/dse1_SoCKitTop/dse1_SoCKitTop_$i"
done
cp BlockRAM.v "dse/dse1_SoCKitTop/dse1_SoCKitTop_base"
cp BlockRAMTrueDual.v "dse/dse1_SoCKitTop/dse1_SoCKitTop_base"
