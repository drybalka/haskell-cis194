skips :: [a] -> [[a]]
skips as = map (\i -> [x | (j, x) <- [1..] `zip` as, j `mod` i == 0]) [1..(length as)]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_, x, _) -> x) (filter (\(a, b, c) -> a < b && b > c) (zip3 xs (drop 1 xs) (drop 2 xs)))

histogram :: [Integer] -> String
histogram xs = concat (transpose (map (\n -> show n ++ "=" ++ replicate (length (filter (==n) xs)) '*') [1..9]))

