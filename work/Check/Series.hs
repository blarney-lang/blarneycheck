module Check.Series where

import Blarney

type Series a = Integer -> [a]

class Serial a where
    series :: Series a


(\/) :: Series a -> Series a -> Series a
s1 \/ s2 = \d -> s1 d ++ s2 d
(><) :: Series a -> Series b -> Series (a, b)
s1 >< s2 = \d -> [(x,y) | x <- s1 d, y <- s2 d]