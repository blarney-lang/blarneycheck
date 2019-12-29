import Blarney
import Blarney.Queue
import Blarney.Stream

makeEcho :: Stream (Bit 8) -> Module (Stream (Bit 8))
makeEcho bytesIn = do
  bytesOut :: Queue (Bit 8) <- makeQueue

  always do
    when (bytesIn.canPeek .&. bytesOut.notFull) do
      enq bytesOut (bytesIn.peek)
      bytesIn.consume

  return (bytesOut.toStream)

main :: IO ()
main = do
  writeVerilogModule makeEcho "Top" "Top-Verilog/"
