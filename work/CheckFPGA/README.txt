This is a sample Quartus and Blarney project for Terasic's SoCKit FPGA.

The Blarney code in "src/" implements a simple "echo" module.  It
consumes bytes on its input stream and echoes them back on its output
stream.

Assuming you've got quartus installed and in your path, and the SoCKit
FPGA is connected to your PC over USB:

  make                   # Build Blarney code and Quartus project
  make download-sof      # Program the FPGA
  nios2-terminal         # Type chars and see them echoed back
