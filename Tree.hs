module Tree 
( Tree(..)
, node
, children
) where

data Tree a = Node a [Tree a] deriving (Show)

node :: Tree a -> a
node (Node node' _) = node'

children :: Tree a -> [Tree a]
children (Node _ trees) = trees
