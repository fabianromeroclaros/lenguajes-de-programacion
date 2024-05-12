module BTreeMerge where
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show, Eq)

instance Functor BinTree where
    fmap :: (a -> b) -> BinTree a -> BinTree b
    fmap _ Empty = Empty
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinTree where
  pure :: a -> BinTree a
  pure a = Node a Empty Empty
  (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Node f leftF rightF) <*> (Node x leftX rightX) = 
    Node (f x) (leftF <*> leftX) (rightF <*> rightX)


-- E 2
treeEg :: BinTree Int
treeEg = Node 5 (Node 10 Empty Empty) (Node 3 Empty Empty)

incrementTreeOne :: Num a => BinTree a -> BinTree a
incrementTreeOne = fmap (+1) 

result :: BinTree Int
result = incrementTreeOne treeEg



-- E 3
treeStr :: BinTree String
treeStr = Node "Hola" (Node "Mundo" Empty Empty) (Node "Game of Thrones" Empty Empty)

convertTreeString :: Num a => BinTree String -> BinTree a
convertTreeString = fmap (fromIntegral . length)

resultTreeStr :: BinTree Int
resultTreeStr = convertTreeString treeStr



-- E 4

mergeTrees :: (Applicative f, Ord a) => BinTree a -> BinTree a -> f (BinTree a)
mergeTrees tree1 tree2 = pure merge <*> pure tree1 <*> pure tree2

merge :: Ord a => BinTree a -> BinTree a -> BinTree a
merge Empty tree = tree
merge tree Empty = tree
merge (Node x left right) tree2 = merge left (merge right (insertTree x tree2))

insertTree :: Ord a => a -> BinTree a -> BinTree a
insertTree x Empty = Node x Empty Empty
insertTree x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (insertTree x left) right
    | x > a  = Node a left (insertTree x right)

tree1 :: BinTree Integer
tree1 = Node 5 (Node 4 Empty Empty) (Node 6 Empty Empty)

tree2 :: BinTree Integer
tree2 = Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)

mergedTree :: (Applicative f) => f (BinTree Integer) 
mergedTree = mergeTrees tree1 tree2


tree3 :: BinTree String
tree3 = Node "hola" (Node "apple" Empty Empty) (Node "grape" Empty Empty)

tree4 :: BinTree String
tree4 = Node "mundo" (Node "cherry" Empty Empty) (Node "orange" Empty Empty)

mergedStringTree :: (Applicative f) => f (BinTree String) 
mergedStringTree = mergeTrees tree3 tree4

-- EXTRA FUNCTION:

treeOp :: Num a => BinTree (a -> a -> a)
treeOp = Node (+) (Node (-) Empty Empty) (Node (*) Empty Empty)

resultF :: BinTree Integer
resultF = treeOp <*> tree1 <*> tree2