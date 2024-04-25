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
removeNs n = removeNsAux n n
    where
        removeNsAux _ _ [] = []
        removeNsAux n 1 (x:xs) = removeNsAux n n xs
        removeNsAux n m (x:xs) = x : removeNsAux n (m - 1) xs

iterateEnum f x = iterateEnumAux f x 0
    where
        iterateEnumAux f x n = x : iterateEnumAux f (f x n) (n + 1)

diag = diagAux 0
    where
        diagAux n (xs:xss) = xs!!n : diagAux (n + 1) xss

nextL xs n = removeNs (xs!!(n + 1)) xs

luckyNumbers = diag (iterateEnum nextL [1,3..])

main :: IO ()
-- main = do putStrLn $ "Alessio Bandiera 1985878"
-- main = do putStrLn $ show $ take 125 insonnia
-- main = do putStrLn $ show $ take 5 tartaglia
main = do putStrLn $ show $ take 50 luckyNumbers
-- main = do putStrLn $ show $ removeNs 2 (take 50 [1..])
-- main = do putStrLn $ show $ take 20 $ nextL [1..] 0
-- main = do putStrLn $ show $ take 20 $ removeNs 9 $ removeNs 7 $ removeNs 3 $ removeNs 2 [1..]
-- main = do putStrLn $ show $ take 20 $ nonLoSo [9, 7, 3, 2] [1..]
