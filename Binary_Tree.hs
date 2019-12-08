module Binary_Tree
where 

data Tree a = Node a (Tree a) (Tree a) | Empty
              deriving(Show)

treeHeight :: (Tree a) -> Int
treeHeight Empty = 0
treeHeight (Node _ q r) = (+1) $ max (treeHeight q) (treeHeight r)

