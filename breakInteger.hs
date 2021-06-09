-- LeetCode Problem 343: Break Integer

main :: IO ()
main = return ()

data SumTree = Leaf Int | SumNode Int SumTree SumTree

breakInteger :: Int -> Int
toTree :: Int -> SumTree
treeProduct :: SumTree -> Int 

breakInteger 2 = 1
breakInteger 3 = 2
breakInteger x = treeProduct(toTree x)

toTree x 
  | leafValues x = Leaf x
  | otherwise = SumNode x (toTree (partition x)) (toTree (partitionRest x))

partition x 
  | mod x 3 == 0 = x - 3
  | otherwise = x - 2

leafValues x = x >= 2 && x <= 3
partitionRest x = x - partition x

treeProduct (Leaf x) = x
treeProduct (SumNode x l r) = (treeProduct l) * (treeProduct r)
