import Blarney

type Property = (String, Prop)
data Prop = Assert (Bit 1) | Forall (Bit 8 -> Prop)

check :: Integer -> Property -> Module ()
check depth (name, property) = always do
  pass <- (checkGenerate 0 name property 1)
  when pass (display "P") >> finish
  where
    checkGenerate _ fName (Assert val) disp = do
      when (val.inv .&. disp) (display "F: " fName)
      return (val .&. disp)
    checkGenerate currVal fName prop@(Forall f) disp = do
      let appName = (fName ++ " " ++ show currVal)
      passed <- checkGenerate 0 appName (f $ fromInteger currVal) disp
      if (currVal >= depth) then return passed else
        checkGenerate (currVal+1) fName prop passed
        
          

firstHot :: Bit 8 -> Bit 8
firstHot x = x .&. (x.inv .+. 1)

top :: Module ()
top = do
  --let prop_HotBitCommon = ("HotBitCommon", Forall \x -> Assert (x .&. (firstHot x) .==. (firstHot x)))
  --check (2^3-1) prop_HotBitCommon
  let prop_SubComm = Forall \x -> Forall \y -> Assert (x.-.y .==. y.-.x)
  check (2^6-1) ("SubCommutativity", prop_SubComm)

main :: IO ()
main = writeVerilogTop top "top" "Out-Verilog/"
