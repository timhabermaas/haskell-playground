import Data.List

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
 | n <= 0    = []
 | n < 10    = [n]
 | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . f . reverse
  where f (x:y:xs) = x:(y * 2):(f xs)
        f xs       = xs


sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
  | x < 9 = x + sumDigits xs
  | otherwise = (sum $ toDigits x) + (sumDigits xs)

validate :: Integer -> Bool
validate n = ((sumDigits . doubleEveryOther . toDigits) n) `rem` 10 == 0

main :: IO ()
main = putStrLn $ show $ validate 4012888888881881
