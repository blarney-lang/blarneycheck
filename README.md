# BlarneyCheck

An exhaustive property-based testing library for
[Blarney](https://github.com/blarney-lang/blarney).

See the associated [project
report](Documentation/blarneycheck:_Property-based_Testing_for_Hardware.pdf)
for additional background and documentation.

## Usage

Make sure to use the `--recursive` flag to include the Blarney repo:
```sh
> git clone --recursive https://github.com/JonasAlaif/blarneycheck.git
```
Alternatively set the `BLARNEY_ROOT` environment variable to point at a copy of Blarney.

The library resides in the 'Haskell' directory and can be imported in any Haskell code:
```hs
import BlarneyCheck
```

To compile a module which imports BlarneyCheck use the modified `blcc` script, instead of `blc`.
Where `blcc` stands for *BlarneyCheck compiler*.

Example modules and associated test benches are provided in the 'Examples' directory.
These can be simulated as a regression test with:
```sh
> ./Test/test.sh
```

Simulation requires GHC 8.6.5 and Verilator to be installed. The examples are also given
as synthesis ready in the 'Synthesizable' directory. Simply:
```sh
> cd Examples/Synthesizable
> make <Insert_Example>.sof
```
This synthesizes the module and test bench. Synthesis requires Quartus to be installed
on the system. To program an FPGA with the test bench run:
```sh
> make download-sof && nios2-terminal
```

