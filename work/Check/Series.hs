module Check.Series where

import Blarney

type Series a = Integer -> [a]

class Serial a where
    series :: Series a


(\/) :: Series a -> Series a -> Series a
s1 \/ s2 = \d -> s1 d ++ s2 d
(><) :: Series a -> Series b -> Series (a, b)
s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]


instance (KnownNat a) => Serial (Bit a) where
    series 0 = [constant 0]
    series d = [new | d > 0, (prev) <- series (d-1), new <- [prev, (prev .|. (1 .<<. (constant (d-1) :: (Bit a))))]]