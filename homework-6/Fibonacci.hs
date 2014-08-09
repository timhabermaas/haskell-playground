fibs = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 50 $ streamToList s

streamToList :: Stream a -> [a]
streamToList (Stream x r) = x:(streamToList r)

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x r) = Stream (f x) (streamMap f r)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: (Stream a) -> (Stream a) -> (Stream a)
interleaveStreams (Stream x rx) (Stream y ry) = Stream x (Stream y (interleaveStreams rx ry))

ruler :: Stream Integer
ruler = streamMap (\n -> last [x | x <- [0..n], rem n (2^x) == 0]) nats

main = putStrLn $ show $ ruler
