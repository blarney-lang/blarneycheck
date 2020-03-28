import Blarney

type Property = (String, Prop)
data Prop = Assert (Bit 1) | Forall (Bit 8 -> Prop)

check :: Integer -> Property -> Module ()
check depth (name, property) = always do
  pass <- (checkGenerate name property 1)
  when pass (display "P") >> finish
  where
    checkGenerate fName (Assert val) pass = do
      when (val.inv .&. pass) (display "F: " fName)
      return (val .&. pass)
    checkGenerate fName (Forall f) pass = do
      let appName = \x -> (fName ++ " " ++ show x)
      feedForward [checkGenerate (appName x) (f $ constant x) | x <- [0 .. depth]] pass
    feedForward [] pass = return pass
    feedForward (tc:tcs) pass = tc pass >>= feedForward tcs
        
          

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
