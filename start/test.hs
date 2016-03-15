data Tree a   = Leaf a
              | Node (Tree a) (Tree a)
              deriving (Show)


treeFold op b (Leaf x)   = b
treeFold op b (Node l r) = (treeFold op b l) `op` (treeFold op b r)
