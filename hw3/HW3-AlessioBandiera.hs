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
diag :: [[a]] -> [a]
diag = diagAux 0
    where
        diagAux n (xs:xss) = xs!!n : diagAux (n + 1) xss

removeNths :: (Eq a, Num a) => a -> [b] -> [b]
removeNths n = removeNthsAux n n
    where
        removeNthsAux _ _ [] = []
        removeNthsAux n 1 (x:xs) = removeNthsAux n n xs
        removeNthsAux n m (x:xs) = x : removeNthsAux n (m - 1) xs

nextL :: (Eq a, Num a) => [a] -> Int -> [a]
nextL xs n = removeNths (xs!!(n + 1)) xs

iterateEnum :: Num b => (a -> b -> a) -> a -> [a]
iterateEnum f x = iterateEnumAux f x 0
    where
        iterateEnumAux f x n = x : iterateEnumAux f (f x n) (n + 1)

luckyNumbers :: [Int]
luckyNumbers = diag (iterateEnum nextL [1,3..])

main :: IO ()
-- main = do putStrLn $ "Alessio Bandiera 1985878"
-- main = do putStrLn $ show $ take 125 insonnia
-- main = do putStrLn $ show $ take 5 tartaglia
main = do putStrLn $ show $ take 50 luckyNumbers
