fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons i s) = i : streamToList s

instance (Show a) => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons i s) = Cons (f i) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons i s1) s2 = Cons i (interleaveStreams s2 s1)

nats = streamFromSeed (+ 1) 0

-- ruler = interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (streamRepeat 3)))

ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  (+) (Cons i s) (Cons j p) = Cons (i + j) (s + p)
  (*) (Cons i s) (Cons j p) = Cons (i * j) ((fromInteger i * p) + (s * Cons j p))
  fromInteger i = Cons i (streamRepeat 0)
  negate = streamMap negate

instance Fractional (Stream Integer) where
  -- fromRational = _
  (/) a@(Cons i s) b@(Cons j p) = Cons (div i j) ((s - (a / b) * p) / fromInteger j)

fibs3 = x / (1 - x - x ^ 2)
