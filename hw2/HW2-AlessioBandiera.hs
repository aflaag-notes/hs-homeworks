import Data.List (sort)

-- ### Esercizio 1.1
merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

couple :: Ord a => [[a]] -> [[a]]
couple [] = []
couple [[x]] = [[x]]
couple [x] = [x]
couple (xs:ys:xss) = merge xs ys : couple xss

isNotSingleton :: [a] -> Bool
isNotSingleton [x] = False
isNotSingleton _ = True

skipWhile :: a -> (a -> Bool) -> [a] -> a
skipWhile def _ [] = def
skipWhile def p (x:xs) = if p x then skipWhile def p xs else x

mergeSort :: Ord a => [a] -> [a]
mergeSort = head . skipWhile [] isNotSingleton . iterate couple . map (\x -> [x])


-- ### Esercizio 1.2
listifyAux :: Ord a => [a] -> ([[a]], [a])
listifyAux [x] = ([], [x])
listifyAux (x:xs) = if x < head last then (out, x : last) else (last : out, [x])
    where
        (out, last) = listifyAux xs

listify :: Ord a => [a] -> [[a]]
listify xs = last : out
    where
        (out, last) = listifyAux xs

mergeSort' :: Ord a => [a] -> [a]
mergeSort' = head . skipWhile [] isNotSingleton . iterate couple . listify


-- ### Esercizio 2.1
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
    deriving Show

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f Empty = Empty
mapBT f (Node a sx dx) = Node (f a) (mapBT f sx) (mapBT f dx)

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf a) = Leaf (f a)
mapBT' f (Node' sx dx) = Node' (mapBT' f sx) (mapBT' f dx)

foldrBT :: (a -> b -> b -> b) -> b -> BinTree a -> b
foldrBT f acc Empty = acc
foldrBT f acc (Node a sx dx) = f a (foldrBT f acc sx) (foldrBT f acc dx)

-- le funzioni fNodes ed fLeaves devono essere separate
-- poiché se la funzione da passare a foldrBT' fosse 1 sola
-- (dunque sia per il caso base che per il caso ricorsivo)
-- questa potrebbe forzare `a` ed `acc` ad avere lo stesso tipo:
-- assumendo che fNodes ed fLeaves siano una sola funzione f,
-- poiché:
--     - è desiderabile che la funzione da applicare all'albero possa
--       interagire sia con `a` che con l'accumulatore del foldrBT';
--     - sono le foglie ad avere le informazioni degli alberi
--       di tipo BinTree' (e dunque a contenere i valori `a`),
--     - nel caso ricorsivo gli argomenti di f possono limitarsi ad essere
--       `acc` oppure un risultato di una chiamata ricorsiva, che deve
--       necessariamente avere comunque lo stesso tipo di `acc`
-- allora, se uno degli argomenti della chiamata di f nel caso ricorsivo
-- è `a`, tale argomento viene forzato ad essere dello stesso tipo
-- di `acc` dalla chiamata di f nel caso ricorsivo; questo comportamento
-- limita la possibilità di effettuare foldrBT'
foldrBT' :: (b -> b -> b) -> (a -> b -> b) -> b -> BinTree' a -> b
foldrBT' fNodes fLeaves acc (Leaf a) = fLeaves a acc
foldrBT' fNodes fLeaves acc (Node' sx dx) = fNodes (foldrBT' fNodes fLeaves acc sx) (foldrBT' fNodes fLeaves acc dx)

foldlBT :: (b -> a -> b) -> b -> BinTree a -> b
foldlBT f acc Empty = acc
foldlBT f acc (Node a sx dx) = foldlBT f (foldlBT f (f acc a) sx) dx

-- vale lo stesso ragionamento di foldrBT'
foldlBT' :: (b -> b) -> (b -> a -> b) -> b -> BinTree' a -> b
foldlBT' fNodes fLeaves acc (Leaf a) = fLeaves acc a
foldlBT' fNodes fLeaves acc (Node' sx dx) = foldlBT' fNodes fLeaves (foldlBT' fNodes fLeaves (fNodes acc) sx) dx


-- ### Esercizio 2.2.a
nodesBT :: BinTree a -> Int
nodesBT b = foldrBT (\a sx dx -> 1 + sx + dx) 0 b

nodesBT' :: BinTree' a -> Int
nodesBT' b = foldrBT' (\sx dx -> sx + dx + 1) (\a acc -> acc) 1 b


-- ### Esercizio 2.2.b
heightBT :: BinTree a -> Int
heightBT b = foldrBT (\a sx dx -> 1 + max sx dx) (-1) b

heightBT' :: BinTree' a -> Int
heightBT' b = foldrBT' (\sx dx -> 1 + max sx dx) (\a acc -> acc) 0 b


-- ### Esercizio 2.2.c
maxUnbalBT :: BinTree a -> Int
maxUnbalBT b = abs (fst fb - snd fb)
    where
    fb = foldrBT (\a (hssx, hsdx) (hdsx, hddx) -> (1 + max hssx hsdx, 1 + max hdsx hddx)) (-1, -1) b

maxUnbalBT' :: BinTree' a -> Int
maxUnbalBT' b = abs (fst fb - snd fb)
    where
        fb = foldrBT' (\(hssx, hsdx) (hdsx, hddx) -> (1 + max hssx hsdx, 1 + max hdsx hddx)) (\a acc -> acc) (0, 0) b


-- ### Esercizio 2.opt
data Tree a = R a [Tree a]
    deriving Show

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R a ts) = R (f a) (map (mapT f) ts)

foldr' f acc [] = acc
foldr' f acc [x] = x
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- per questa funzione è necessario prendere in input due funzioni,
-- una per descrivere come gestire il comportamento tra i valori
-- salvati nei nodi dell'albero rispetto all'accumulatore, l'altra
-- per descrivere come combinare i risultati dei figli; inoltre,
-- è necessario utilizzare una versione modificata di foldr'
-- o alternativamente prendere in input due casi base diversi
-- per gli accumulatori delle due funzioni, altrimenti non è possibile
-- scrivere alcune funzioni (ad esempio il massimo sbilanciamento)
foldrT :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree a -> b
foldrT fNodes fLists acc (R a ts) = fNodes a (foldr' fLists acc (map (foldrT fNodes fLists acc) ts))

foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f acc (R a ts) = foldl (\acc tss -> foldlT f acc tss) (f acc a) ts

nodesT :: Tree a -> Int
nodesT t = foldrT (\a acc -> acc + 1) (+) 0 t

heightT :: Tree a -> Int
heightT t = foldrT (\a acc -> acc + 1) (\x acc -> max x acc) (-1) t

maxUnbalT :: Tree a -> Int
maxUnbalT t = abs (fst ft - snd ft)
    where
        ft = foldrT (\a (accMin, accMax) -> (1 + accMin, 1 + accMax)) (\(xMin, xMax) (accMin, accMax) -> (min xMin accMin, max xMax accMax)) (-1, -1) t


-- ### Esercizio 3
-- T(n) = T(k) + T(n - k - 1) + O(n) => O(n log n)
balancedNodesAux :: Int -> BinTree Int -> ([Int], Int)
balancedNodesAux n Empty = ([], 0)
balancedNodesAux n (Node a sx dx) = if n == totalSum then (a : totalNodes, totalSum) else (totalNodes, totalSum)
    where
        (sxNodes, sxSum) = balancedNodesAux (n + a) sx
        (dxNodes, dxSum) = balancedNodesAux (n + a) dx
        totalNodes = sxNodes ++ dxNodes
        totalSum = sxSum + dxSum + a

balancedNodes :: BinTree Int -> [Int]
balancedNodes b = fst (balancedNodesAux 0 b)


-- ### Esercizio 4
-- O(n)
orderedDedup :: Ord a => [a] -> [a]
orderedDedup xs = head xs : map snd (filter (uncurry (/=)) (zip xs (tail xs)))

-- 2T(n/2) + O(n) => T(n) = O(n log n)
listToABRAux :: Ord a => [a] -> BinTree a
listToABRAux [] = Empty
listToABRAux [x] = Node x Empty Empty
listToABRAux xs = Node root (listToABRAux left) (listToABRAux right)
    where
        (left, rootedRight) = splitAt (length xs `div` 2) xs
        root = head rootedRight
        right = tail rootedRight

-- O(n log n) + O(n) + O(n log n) = O(n log n)
-- che risulta essere una complessità ottima rispetto al problema
-- poiché l'ABR costruito è bilanciato, in quanto l'altezza dell'albero
-- è in O(log n) per via di come l'ABR viene costruito dalla funzione
-- `listToABRAux`, che divide ogni lista di in input in 2 sottoliste
-- di stessa lunghezza
listToABR :: Ord a => [a] -> BinTree a
listToABR = listToABRAux . orderedDedup . sort


-- ### Esercizio 5
-- definizioni:
-- head (x:xs) = x
--
-- map f [] = []
-- map f (x:xs) = f x : map f xs
-- 
-- tails [] = [[]]
-- tails (x:xs) = (x:xs) : tails xs
--
-- foldr f e [] = e
-- foldr f e (x:xs) = f x (foldr f e xs)
--
-- scanr f e = map (foldr f e) . tails
-- 
--
-- lemma 1:
--     head . map f = f . head
-- 
-- dimostrazione:
--     head . map f (x:xs) =
--     head (map f (x:xs)) = {def di .}
--     head (f x : map f xs) = {def di map}
--     f x = {def di head}
--     f (head (x:xs)) = {def di head}
--     (f . head) (x:xs) {def di .}
--
--
-- lemma 2:
--     foldr f e = head . scanr f e
-- 
-- dimostrazione:
-- caso []:
--      head . scanr f e []
--      head (map (foldr f e) . tails []) = {def di scanr}
--      head (map (foldr f e) (tails [])) = {def di .}
--      head (map (foldr f e) [[]]) = {def di tails}
--      head ([foldr f e []]) = {def di map}
--      head ([e]) = {def foldr}
--      e = {def di head}
--      foldr f e {def di foldr}
--
-- caso (x:xs)
--      head . scanr f e (x:xs) =
--      head (map (foldr f e) . tails (x:xs)) = {def di scanr}
--      head (map (foldr f e) (tails (x:xs))) = {def di .}
--      head (map (foldr f e) ((x:xs) : tails xs)) = {def di tails}
--      (head . map (foldr f e)) ((x:xs) : tails xs) = {def di .}
--      ((foldr f e) . head) ((x:xs) : tails xs) = {lemma 1}
--      foldr f e (head ((x:xs) : tails xs)) = {def di .}
--      foldr f e (x:xs) {def di head}
--
--
-- derivazione di scanr lineare:
-- caso []:
--      scanr f e [] =
--      map (foldr f e) . tails [] = {def di scanr}
--      map (foldr f e) (tails []) = {def di .}
--      map (foldr f e) [[]] = {def di tails}
--      [foldr f e []] = {def di map}
--      [e] {def di foldr}
--
-- caso (x:xs):
--      scanr f e (x:xs) =
--      map (foldr f e) . tails (x:xs) = {def di scanr}
--      map (foldr f e) (tails (x:xs)) = {def di .}
--      map (foldr f e) ((x:xs) : tails xs) = {def di tails}
--      foldr f e (x:xs) : map (foldr f e) (tails xs) = {def di map}
--      foldr f e (x:xs) : map (foldr f e) . tails xs = {def di .}
--      foldr f e (x:xs) : scanr f e xs = {def di scanr}
--      f x (foldr f e xs) : scanr f e xs = {def di foldr}
--      f x ((head . scanr f e) xs) : scanr f e xs {lemma 2}
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f e [] = [e]
scanr' f e (x:xs) = f x (head sxs) : sxs
    where
        sxs = scanr' f e xs


main :: IO ()
-- main = do putStrLn $ show $ mergeSort [5, 3, 4, 2, 1, 6, 8, 7, 0]
-- main = do putStrLn $ show $ mergeSort' [7, 8, 9, 1, 2, 3]
-- main = do putStrLn $ show $ mapBT (+3) (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ mapBT' (+3) (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ foldrBT (\acc sx dx -> acc + sx + dx) 0 (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ foldrBT' (\sx dx -> sx + dx + 1) (\a acc -> acc) 1 (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ foldlBT (\acc a -> acc + 1) 0 (Node 'a' (Node 'a' Empty Empty) (Node 'a' (Node 'a' (Node 'a' Empty Empty) Empty) Empty))
-- main = do putStrLn $ show $ foldlBT' (\u -> u + 1) (\acc a -> acc + 1) 0 (Node' (Node' (Leaf 1) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ nodesBT (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ nodesBT' (Node' (Node' (Leaf 10) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ heightBT (Node 1 (Node 2 Empty Empty) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ heightBT' (Node' (Node' (Leaf 10) (Leaf 2)) (Node' (Node' (Leaf 3) (Leaf 4)) (Leaf 5)))
-- main = do putStrLn $ show $ maxUnbalBT (Node 1 (Node 2 (Node 2 Empty Empty) Empty) (Node 3 (Node 4 Empty Empty) (Node 5 (Node 6 Empty (Node 7 Empty (Node 8 Empty Empty))) Empty)))
-- main = do putStrLn $ show $ maxUnbalBT' (Node' (Node' (Node' (Leaf 1) (Leaf 2)) (Leaf 3)) (Node' (Node' (Leaf 4) (Leaf 5)) (Node' (Node' (Leaf 6) (Node' (Leaf 7) (Node' (Leaf 8) (Leaf 9)))) (Leaf 10))))
-- main = do putStrLn $ show $ mapT (+1) (R 1 [R 2 [R 6 [R 7 []]], R 3 [], R 4 [R 5 []]])
-- main = do putStrLn $ show $ foldrT (\a acc -> a + acc) (\a acc -> a + acc) 0 (R 1 [R 2 [R 6 [R 7 []]], R 3 [], R 4 [R 5 []]])
-- main = do putStrLn $ show $ foldlT (+) 0 (R 1 [R 2 [R 6 [R 7 []]], R 3 [], R 4 [R 5 []]])
-- main = do putStrLn $ show $ nodesT (R 1 [R 2 [R 6 [R 7 []]], R 3 [], R 4 [R 5 []]])
-- main = do putStrLn $ show $ heightT (R 1 [R 2 [R 6 [R 7 []]], R 3 [], R 4 [R 5 []]])
main = do putStrLn $ show $ maxUnbalT (R 1 [R 2 [R 8 []], R 3 [], R 5 [R 6 [R 7 []]]])
-- main = do putStrLn $ show $ maxUnbalT (R 1 [R 2 [R 3 []], R 4 []])
-- main = do putStrLn $ show $ balancedNodes (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty Empty)) (Node 3 (Node 4 Empty Empty) Empty))
-- main = do putStrLn $ show $ listToABR [5, 2, 7, 8, 2, 2, 7, 2, 7, 1, 5]
-- main = do putStrLn $ show $ scanr' (+) 0 [1, 2, 3]
