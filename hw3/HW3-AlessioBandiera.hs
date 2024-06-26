import Data.List (intercalate)

-- ### Esercizio 1
insonnia :: [Char]
insonnia = intercalate " sheep " (map show [1..])


-- ### Esercizio 2
nextT :: [Int] -> [Int]
nextT xs = (1 : zipWith (+) xs (tail xs)) ++ [1]

tartaglia :: [[Int]]
tartaglia = [1] : map nextT tartaglia


-- ### Esercizio 3
luckyNumbers = 1 : luckyNumbersAux [1,3..] 1
    where
        luckyNumbersAux xs idx = newLucky : luckyNumbersAux (map fst (filter (\(x, i) -> i `mod` newLucky /= 0) (zip xs [1..]))) (idx + 1)
            where
                newLucky = xs!!idx


-- ### Esercizio 1D.1
primRec :: (Eq a, Num a) => (a -> b -> b) -> b -> a -> b
primRec h g 0 = g
primRec h g n = h (n - 1) (primRec h g (n - 1))


-- ### Esercizio 1D.2
primRec' :: (Eq a, Num a) => (a -> b -> b) -> b -> a -> b
primRec' h g n = snd ((for (\(x, y) -> (x + 1, h x y)) n) (0, g))
    where
        for f 0 = \x -> x
        for f n = f . (for f (n - 1))


-- ### Esercizio 1D.3
-- è possibile definire la ricorsione primitiva
-- attraverso i numerali di Church, poiché dato un certo
-- 
-- nChurch = \x y -> x ( ... ( x y ))     [assumiamo che x sia composta n volte]
-- 
-- è possibile utilizza la funzione utilizzata per l'esercizio
-- precedente come segue
--
-- primRec h g n = snd (nChurch (\(x, y) -> (x + 1, h x y)) (0, g))
--               = snd ((\(x, y) -> (x + 1, h x y)) ... (\(x, y) -> (x + 1, h x y)) (0, g))    [la funzione sarà composta su sé stessa n volte]
--               = snd (for (\(x, y) -> (x + 1, h x y)) (0, g))
--
-- per definizione di for; infatti, i numerali di Church costituiscono
-- di fatto degli iteratori, componendo una data funzione con sé stessa n volte


-- ### Esercizio 1D.4
-- consideriamo il tipo induttivo dei naturali
-- definito come segue (ho utilizzato questo per semplicità
-- di ragionamento)
data Nats = S Nats | Z
    deriving (Eq, Show)

fromNat :: (Num a, Eq a) => Nats -> a
fromNat Z = 0
fromNat (S x) = 1 + fromNat x

intoNat :: (Num a, Eq a) => a -> Nats
intoNat 0 = Z
intoNat x = S (intoNat (x - 1))

-- e sia primRec su Nats definita come segue
primRecNats :: a -> (Nats -> a -> a) -> Nats -> a
primRecNats a h Z = a
primRecNats a h (S n) = h n (primRecNats a h n)

-- dunque la funzione di Ackermann
-- su Nats risulta essere definita come segue
ackermann :: Nats -> Nats -> Nats
ackermann Z n = S n
ackermann (S m) Z = ackermann m (S Z)
ackermann (S m) (S n) = ackermann m (ackermann (S m) n)

-- ma poiché il terzo caso di definizione della funzione di Ackermann
-- vede in input sia il successore di m che il successore di n,
-- per scrivere un termine che definisca la funzione di Ackermann
-- attraverso la ricorsione primitiva sarebbero necessari almeno
-- due utilizzi di primRecNats al fine di ottenere m ed n singolarmente,
-- dunque ho preferito scomporre la funzione di Ackermann per semplicità, come segue
ackermannSplit :: Nats -> Nats -> Nats
ackermannSplit Z y = S y
ackermannSplit (S m) y = ackermannSplitAux y m
    where
        ackermannSplitAux Z b = ackermannSplit b (S Z)
        ackermannSplitAux (S n) b = ackermannSplit b (ackermannSplit (S b) n)

-- e le due definizioni sono equivalenti, infatti
-- 
-- ackermannSplit Z y = S y = ackermann Z y
-- ackermannSplit (S m) Z = ackermannSplitAux Z m
--                        = ackermannSplit m (S Z)
--                        = ackermann (S m) Z
-- ackermannSplit (S m) (S n) = ackermannSplitAux (S n) m
--                            = ackermannSplit m (ackermannSplit (S m) n)
--                            = ackermann (S m) (S n)

-- allora è possibile definire i seguenti lambda termini
-- per poter definire la funzione di Ackermann attraverso primRecNats
ackermannSplit' :: Nats -> Nats -> Nats
ackermannSplit' = primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a)
    where
        ackermannSplitAux' = primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p))

-- e se ne dimostra facilmente la correttezza, infatti
--
-- ackermannSplit' Z y = primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) Z y
--                     = (\z -> S z) y
--                     = S y
--
-- ackermannSplit' (S m) y = primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) (S m) y
--                         = (\a b c -> ackermannSplitAux' c a) m (primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) m) y
--                         = ackermannSplitAux' y m
--
-- ed esiste ed è unico l'omomorfismo tale che
--
-- primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) Z = \z -> S z
-- primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) (S n) = (\a b c -> ackermannSplitAux' c a) n (primRecNats (\z -> S z) (\a b c -> ackermannSplitAux' c a) n)
-- 
-- in quanto
--
-- (\z -> S z) ha tipo Nats -> Nats
-- (\a b c -> ackermannSplitAux' c a) ha tipo Nats -> b -> Nats -> Nats
--
-- analogamente, per l'altro lambda termine si ottiene che
--
-- ackermannSplitAux' Z b = primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) Z b
--                        = (\z -> ackermannSplit' z (S Z)) b
--                        = ackermannSplit' b (S Z)
--
-- ackermannSplitAux' (S n) b = primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) (S n) b
--                            = (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) n (primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) n) b
--                            = ackermannSplit' b (ackermannSplit' (S b) n)
--
-- ed esiste ed è unico l'omomorfismo tale che
--
-- primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) Z = \z -> ackermannSplit' z (S Z)
-- primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) (S n) = (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) n (primRecNats (\z -> ackermannSplit' z (S Z)) (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) n)
--
-- in quanto
--
-- (\z -> ackermannSplit' z (S Z)) ha tipo Nats -> Nats
-- (\p q r -> ackermannSplit' r (ackermannSplit' (S r) p)) ha tipo Nats -> q -> Nats -> Nats


-- ### Esercizio 2D.1
partsFromAll :: Int -> [[Int]] -> [[Int]]
partsFromAll n xss = (takeWhile (\xs -> n /= head xs) (map (\xs -> take (length (takeWhile (\x -> x < n) (scanl (+) 0 xs))) xs) xss)) ++ [[n]]


-- ### Esercizio 2D.2
addOneToLast :: (Num a) => [a] -> [a]
addOneToLast [] = []
addOneToLast [x] = [x + 1]
addOneToLast (x:xs) = x : addOneToLast xs

isDescending :: (Ord a) => [a] -> Bool
isDescending [] = True
isDescending [x] = True
isDescending (x:y:xs) = if x < y then False else isDescending (y:xs)

allPartitions :: [[Int]]
allPartitions = (repeat 1) : allPartitionsAux 2
    where
        allPartitionsAux n = map (++ repeat 1) (filter isDescending (map addOneToLast (partsFromAll (n - 1) allPartitions))) ++ allPartitionsAux (n + 1)


-- ### Esercizio 2D.3
powersetN :: [[Int]]
powersetN = [] : powersetNAux 1
    where
        powersetNAux n = map (n:) (takeWhile (\xs -> (xs == []) || (n > head xs)) powersetN) ++ powersetNAux (n + 1)

interleave :: a -> [[a]] -> [[a]]
interleave n [] = []
interleave n (xs:xss) = interleaveAux n [] xs ++ interleave n xss
    where
        interleaveAux n xs [] = [xs ++ [n]]
        interleaveAux n xs (y:ys) = (xs ++ [n] ++ (y:ys)) : interleaveAux n (xs ++ [y]) ys

permutationsN :: [[Int]]
permutationsN = [] : permutationsNAux 1
    where
        permutationsNAux n = interleave n (takeWhile (\xs -> (xs == []) || (n > head xs)) permutationsN) ++ permutationsNAux (n + 1)


-- ### Esercizio 3D.1
allSums :: (Num a) => [a] -> [[a]]
allSums [] = []
allSums (x:xs) = map (x+) xs : allSums xs

diags :: [[a]] -> [[a]]
diags (xs:xss) = zipWith (:) xs ([] : diags xss)

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

removeDups :: (Num a, Eq a) => [a] -> [a]
removeDups xs = removeDupsAux 1 (head xs) (tail xs)
    where
        removeDupsAux n x [] = if n == 1 then [x] else []
        removeDupsAux n x (y:xs) = if x == y then removeDupsAux (n + 1) x xs else (if n == 1 then x : removeDupsAux 1 y xs else removeDupsAux 1 y xs)

ulams :: [Int]
ulams = 1 : 2 : ulamNumbers 1 2
    where
        ulamNumbers n x = nextU : ulamNumbers (n + 1) nextU
            where
                mergedDiags = foldl1 merge (take n (diags (allSums ulams)))
                nextU = head (filter (\y -> y > x) (removeDups mergedDiags))


-- ### Esercizio 4D.1
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving (Eq, Show)

calkinWilf :: BinTree (Int, Int)
calkinWilf = calkinWilfAux 1 1
    where
        calkinWilfAux m n = Node (m, n) (calkinWilfAux m (m + n)) (calkinWilfAux (m + n) n)


-- ### Esercizio 4D.2
takeNlevels :: Int -> BinTree b -> BinTree b
takeNlevels 0 _ = Empty
takeNlevels n (Node a l r) = Node a (takeNlevels (n - 1) l) (takeNlevels (n - 1) r)


-- ### Esercizio 4D.3
visitaLivelli :: (Eq a) => BinTree a -> [a]
visitaLivelli Empty = []
visitaLivelli (Node a l r) = a : visitaLivelliAux [l, r]
    where
        visitaLivelliAux [] = []
        visitaLivelliAux ((Node a l r):qs) = a : visitaLivelliAux (qs ++ (filter (\x -> x /= Empty) [l, r]))


main :: IO ()
main = do putStrLn $ "Alessio Bandiera 1985878"
