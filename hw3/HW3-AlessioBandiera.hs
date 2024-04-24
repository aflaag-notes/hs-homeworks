import Data.List (intercalate)

-- ### Esercizio 1
insonnia :: [Char]
insonnia = intercalate " sheep " (map show [1..])


-- ### Esercizio 2
nextT :: [Int] -> [Int]
nextT xs = (1 : zipWith (+) xs (tail xs)) ++ [1]

tartaglia :: [[Int]]
tartaglia = [1] : map nextT tartaglia


-- Esercizio 3
removeNs n = removeNsAux n n
    where
        removeNsAux _ _ [] = []
        removeNsAux n 1 (x:xs) = removeNsAux n n xs
        removeNsAux n m (x:xs) = x : removeNsAux n (m - 1) xs

luckyNumbers = luckyNumbersAux idxs [1..]
    where
        idxs = reverse (2 : tail luckyNumbers)
        luckyNumbersAux [] ys = ys
        luckyNumbersAux (x:xs) ys = removeNs x (luckyNumbersAux xs ys)

main :: IO ()
-- main = do putStrLn $ "Alessio Bandiera 1985878"
-- main = do putStrLn $ show $ take 125 insonnia
-- main = do putStrLn $ show $ take 5 tartaglia
main = do putStrLn $ show $ take 2 luckyNumbers
-- main = do putStrLn $ show $ removeNs 2 (take 50 [1..])
-- main = do putStrLn $ show $ take 20 $ nextL 3 $ nextL 2 (nextL 2 [1..])
-- main = do putStrLn $ show $ take 20 $ removeNs 9 $ removeNs 7 $ removeNs 3 $ removeNs 2 [1..]
-- main = do putStrLn $ show $ take 20 $ nonLoSo [9, 7, 3, 2] [1..]
