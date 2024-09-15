fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

treeDepth :: Tree a -> Integer
treeDepth Leaf = 0
treeDepth (Node depth _ _ _) = depth

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree (x : xs) = Node ((treeDepth left `max` treeDepth right) + 1) left x right
  where
    half = length xs `div` 2
    left = foldTree (take half xs)
    right = foldTree (drop half xs)

xor :: [Bool] -> Bool
xor = foldr (\a b -> (a && not b) || (not a && b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\n -> 2*n + 1) (filter (`notElem` map (\(i, j) -> i + j + 2 * i * j) (cartProd [1 .. n] [1 .. n])) [1 .. (2 * n + 2)])
