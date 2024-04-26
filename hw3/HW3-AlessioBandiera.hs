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
removeNths :: (Eq a, Num a) => a -> [b] -> [b]
removeNths n = removeNthsAux n n . tail
    where
        removeNthsAux _ _ [] = []
        removeNthsAux n 1 (x:xs) = removeNthsAux n n xs
        removeNthsAux n m (x:xs) = x : removeNthsAux n (m - 1) xs

luckyNumbers = 1 : luckyNumbersAux [3,5..] 3
    where
        luckyNumbersAux (x:xs) i = x : luckyNumbersAux (fs ++ removeNths x rs) (i + 1)
            where
                (fs, rs) = splitAt (x - i) xs


-- ### Esercizio 1D.1
data Nats = S Nats | Z
    deriving (Eq, Show)

-- f Z y = S y
-- f (S m) y = g y m

g Z b = f b (S Z)
g (S n) b = f b (f (S b) n)

primRec a h Z = a
primRec a h (S n) = h n (primRec a h n)

f = \x w -> (primRec (\z -> S z) (\a b c -> g c a) x w)

-- ### Esercizio 2D.3
nextP n xs = [] : (filter (\x -> x /= []) xs) ++ map (n:) xs

powersetN = powersetNAux 1
    where
        powersetNAux n = [] : map (nextP n) (powersetNAux (n + 1))


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
visitaLivelli :: Eq a => BinTree a -> [a]
visitaLivelli Empty = []
visitaLivelli (Node a l r) = a : visitaLivelliAux [l, r]
    where
        visitaLivelliAux [] = []
        visitaLivelliAux ((Node a l r):qs) = a : visitaLivelliAux (qs ++ (filter (\x -> x /= Empty) [l, r]))


main :: IO ()
-- main = do putStrLn $ "Alessio Bandiera 1985878"
-- main = do putStrLn $ show $ take 125 insonnia
-- main = do putStrLn $ show $ take 5 tartaglia
-- main = do putStrLn $ show $ take 50 luckyNumbers
-- main = do putStrLn $ show $ visitaLivelli (takeNlevels 4 calkinWilf)
main = do putStrLn $ show $ take 10 powersetN
