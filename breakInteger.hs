-- LeetCode Problem: Break Integer

data SumTree = Leaf Int | SumNode Int SumTree SumTree deriving Show

breakInteger :: Int -> Int
breakInteger 2 = 1
breakInteger 3 = 2
breakInteger x = treeProduct(toTree x)

toTree :: Int -> SumTree
toTree x 
  | leafValues x = Leaf x
  | otherwise = SumNode x (toTree (partition x)) (toTree (partitionRest x))

leafValues x = x >= 2 && x <= 3

partition x 
 | x `mod` 3 == 0 = x - 3
 | otherwise = x - 2
 
partitionRest x = x - partition x

treeProduct :: SumTree -> Int 
treeProduct (Leaf x) = x
treeProduct (SumNode x l r) = (treeProduct l) * (treeProduct r)
