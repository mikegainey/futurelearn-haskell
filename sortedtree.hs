-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show


treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

--------------------------------------------------------------------------------

unsortedTree = Node 3 (Node 4 (Node 7 Leaf Leaf) Leaf) (Node 6 Leaf (Node 5 Leaf Leaf))
sortedTree = Node 50 (Node 40 (Node 35 Leaf Leaf) (Node 45 Leaf Leaf)) (Node 60 (Node 55 Leaf Leaf) (Node 65 Leaf Leaf))

isSortedTree2 :: Tree -> Bool
-- return True if the given tree is sorted
isSortedTree2 Leaf = True
isSortedTree2 (Node val left right) = sortedless left val && sortedmore right val
  where sortedless :: Tree -> Int -> Bool
        -- return True if all Nodes in the Tree have values <= the given Int
        sortedless Leaf x = True
        sortedless (Node val ltree rtree) x = (val <= x) && sortedless ltree x && sortedless rtree x

        sortedmore :: Tree -> Int -> Bool
        -- return True if all Nodes in the Tree have values >= the given Int
        sortedmore Leaf x = True
        sortedmore (Node val ltree rtree) x = (val >= x) && sortedmore ltree x && sortedmore rtree x

insert :: Tree -> Int -> Tree
-- given a Tree and Int, return a new Tree with the Int inserted
insert Leaf val = Node val Leaf Leaf
insert (Node nval left right) val =
  if val <= nval
     then Node nval (insert left val) right
     else Node nval left (insert right val)

tree2list :: Tree -> [Int]
-- given a Tree, return a list of the tree's values
tree2list Leaf = []
tree2list (Node val left right) = tree2list left ++ (val : tree2list right)
-- After seeing CoderDennis's version, I rearranged the terms to return the values in order.
