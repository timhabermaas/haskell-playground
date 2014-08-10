{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Stream
import Data.Ratio

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

mult :: Stream Integer -> Stream Integer -> Stream Integer
mult (Stream x xr) ya@(Stream y yr) = Stream (x * y) ((streamMap (* x) yr) + mult xr ya)

instance Num (Stream Integer) where
  fromInteger n = Stream n (streamRepeat 0)
  negate      s = streamMap negate s
  (+)           = streamZip (+)
  (*)           = mult


instance Fractional (Stream Integer) where
  (/) x@(Stream xe xr) y@(Stream ye yr) = Stream (xe `div` ye) rest
    where rest  = streamMap (\n -> (if ye == 0 then 0 else n `div` ye)) rest
          rest' = yr + (negate (rest * yr))

main = putStrLn $ show $ (x^2 + x) / 2
