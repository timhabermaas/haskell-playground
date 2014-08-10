import Stream

fibs = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (\n -> last [x | x <- [0..n], rem n (2^x) == 0]) nats

main = putStrLn $ show $ ruler
