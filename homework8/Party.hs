module Party where

import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL el fun) = GL (e : el) (fun + empFun e)

instance Semigroup GuestList where
  (<>) (GL e1 f1) (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ fl) r@(GL _ fr) = if fl > fr then l else r

treeFold :: (Monoid b) => (a -> [b] -> b) -> Tree a -> b
treeFold f (Node {rootLabel = rl, subForest = sf}) = f rl (map (treeFold f) sf)

nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel e glp = (glCons e (foldMap snd glp), foldMap (uncurry moreFun) glp)

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t

main :: IO ()
main = readFile "company.txt" >>= (print . maxFun . read)
