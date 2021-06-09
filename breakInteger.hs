-- LeetCode Problem: Break Integer
-- Runnable at https://replit.com/@dbizchallenge/BreakInteger-Haskell#main.hs

import Data.Tree hiding (Tree)

main :: IO ()
main = return ()

data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving Show

breakInteger :: Int -> Int
breakInteger 2 = 1
breakInteger 3 = 2
breakInteger x = treeProduct(toTree x)

toTree :: Int -> Tree Int
toTree x 
  | leafValues x = Leaf x
  | otherwise = Branch x (toTree (partition x)) (toTree (partitionRest x))

leafValues x = x >= 2 && x <= 3

partition x 
 | x `mod` 3 == 0 = x - 3
 | otherwise = x - 2
 
partitionRest x = x - partition x

treeProduct :: Tree Int -> Int 
treeProduct (Leaf x) = x
treeProduct (Branch x l r) = (treeProduct l) * (treeProduct r)

-- Display Tree
toPrintable :: Int -> Tree String
toPrintable x 
  | leafValues x = Leaf (show x)
  | otherwise = Branch (show x) (toPrintable (partition x)) (toPrintable (partitionRest x))

toDataTree (Leaf x) = Node x []
toDataTree (Branch x l r) = Node x [toDataTree l, toDataTree r]

printTree i = putStrLn $ drawTree (toDataTree (toPrintable i))
