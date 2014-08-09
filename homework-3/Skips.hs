import Test.QuickCheck

nthElements :: [a] -> Int -> [a]
nthElements xs n
  | (length xs) >= n = (last $ take n xs):(nthElements (drop n xs) n)
  | otherwise        = []

skips :: [a] -> [[a]]
skips xs = take (length xs) $ map (nthElements xs) [1..]

testSkipsLength :: [Int] -> Bool
testSkipsLength xs = (length $ skips xs) == length xs

testSkipsFirst :: [Int] -> Bool
testSkipsFirst [] = True
testSkipsFirst xs = xs == (head $ skips xs)

main = do
  quickCheck testSkipsLength
  quickCheck testSkipsFirst
  putStrLn $ show $ skips [1..4]
