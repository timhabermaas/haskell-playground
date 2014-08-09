localMaxima' :: [Integer] -> [Integer] -> [Integer]
localMaxima' (x:y:z:ls) m
  | y > x && y > z = localMaxima' (y:z:ls) (m ++ [y])
  | otherwise      = localMaxima' (y:z:ls) m
localMaxima' ls m = m

localMaxima :: [Integer] -> [Integer]
localMaxima xs = localMaxima' xs []

main = putStrLn $ show $ localMaxima [1, 4, 3]
