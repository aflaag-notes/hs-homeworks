data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving Show

--- ### Esercizio 2.1
mapBT f Empty = Empty
mapBT f (Node a sx dx) = Node (f a) (mapBT f sx) (mapBT f dx)

mapBT' f (Leaf a) = Leaf (f a)
mapBT' f (Node' sx dx) = Node' (mapBT' f sx) (mapBT' f dx)

foldrBT f acc Empty = acc
foldrBT f acc (Node a sx dx) = f a (foldrBT f acc sx) (foldrBT f acc dx)

foldrBT' f acc (Leaf a) = f a acc
foldrBT' f acc (Node' sx dx) = f (foldrBT' f acc sx) (foldrBT' f acc dx)

main :: IO ()
-- main = do putStrLn $ show $ mapBT (+3) (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ mapBT' (+3) (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ foldrBT (\x y z -> x + y + z) 0 (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
main = do putStrLn $ show $ foldrBT' (+) 0 (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
