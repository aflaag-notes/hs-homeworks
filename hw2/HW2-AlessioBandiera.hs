-- ### Esercizio 1.1
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

couple [] = []
couple [[x]] = [[x]]
couple [x] = [x]
couple (xs:ys:xss) = merge xs ys : couple xss

isSingleton [x] = False
isSingleton _ = True

skipWhile _ [] = []
skipWhile p (x:xs) = if p x then skipWhile p xs else x

-- TODO: non capisco perché con la lista vuota non funziona
mergeSort xs = head $ skipWhile isSingleton (iterate couple (map (\x -> [x]) xs))


-- ### Esercizio 2.2
-- TODO: da fare


-- ### Esercizio 2.1
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving Show

mapBT f Empty = Empty
mapBT f (Node a sx dx) = Node (f a) (mapBT f sx) (mapBT f dx)

mapBT' f (Leaf a) = Leaf (f a)
mapBT' f (Node' sx dx) = Node' (mapBT' f sx) (mapBT' f dx)

foldrBT f acc Empty = acc
foldrBT f acc (Node a sx dx) = f a (foldrBT f acc sx) (foldrBT f acc dx)

foldrBT' f acc (Leaf a) = f a acc
foldrBT' f acc (Node' sx dx) = f (foldrBT' f acc sx) (foldrBT' f acc dx)

foldlBT f acc Empty = acc
foldlBT f acc (Node a sx dx) = foldlBT f (foldlBT f (f acc a) sx) dx

foldlBT' f acc (Leaf a) = f a acc
foldlBT' f acc (Node' sx dx) = foldlBT' f (foldlBT' f acc sx) dx


-- ### Esercizio 2.2
-- TODO: È SBAGLIATA
nodes b = foldrBT (\acc sx dx -> acc + 1) 0 b


-- ### Esercizio 3
-- T(n) = T(k) + T(n - k - 1) + O(n)
balancedNodesAux n Empty = ([], 0)
balancedNodesAux n (Node a sx dx) = if n == totalSum then (a : totalNodes, totalSum) else (totalNodes, totalSum)
    where (sxNodes, sxSum) = balancedNodesAux (n + a) sx
          (dxNodes, dxSum) = balancedNodesAux (n + a) dx
          totalNodes = sxNodes ++ dxNodes
          totalSum = sxSum + dxSum + a

balancedNodes b = fst (balancedNodesAux 0 b)


main :: IO ()
-- main = do putStrLn $ show $ mergeSort [5, 3, 4, 2, 1, 6, 8, 7, 0]
-- main = do putStrLn $ show $ mergeSort [1, 2]
-- main = do putStrLn $ show $ mapBT (+3) (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ mapBT' (+3) (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ foldrBT (\acc sx dx -> acc + sx + dx) 0 (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ foldrBT' (+) 0 (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ foldlBT (+) 0 (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
main = do putStrLn $ show $ foldlBT' (+) 0 (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ nodes (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ balancedNodes (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty Empty)) (Node 3 (Node 4 Empty Empty) Empty))
