BLARNEYCHECK
------------

An exhaustive property-based testing library for Blarney. Make sure to include
the 'blarney' folder in this directory:
$ git clone https://github.com/mn416/blarney.git

The library resides in the 'Haskell' directory and can be imported in any Haskell code:
import BlarneyCheck

Example modules and associated test benches are provided in the 'BuggySuite' folder.
These can be simulated using the following command:
$ ./simulate.sh BuggySuite/<Insert_Example>.hs

Simulation requires GHC 8.6.5 and Verilator to be installed. The examples are also given
as synthesis ready in the corresponding 'CheckFPGA_<Insert_Example>' directory. Simply:
$ cd CheckFPGA_<Insert_Example>
$ make

This synthesizes the module and test bench. Synthesis requires Quartus to be installed
on the system. To program an FPGA with the test bench run:
$ make download-sof
$ nios2-terminal

