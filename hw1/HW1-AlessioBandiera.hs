-- ### Esercizio 1.1
myTakeWhile p [] = []
myTakeWhile p (x:xs) = x : if p x then myTakeWhile p xs else []

myDropWhile p [] = []
myDropWhile p xs@(x:txs) = if p x then myDropWhile p txs else xs


-- ### Esercizio 1.2
-- O(n)
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd xs
    | zipped == [] = []
    | otherwise = fst zippedHead : snd zippedHead : map snd (tail zipped)
    where zipped = filter (uncurry $ \x y -> x /= y) (zip xs (tail xs))
          zippedHead = head zipped


-- ### Esercizio 1.3
-- TODO: da fare


-- ### Esercizio 2.1
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []

myZipWith1 f xs ys = zapp (map f xs) ys


-- ### Esercizio 2.2
myZipWith2 f xs ys = map (uncurry f) (zip xs ys)


-- ### Esercizio 2.3
myMap1 f = foldr ((:) . f) []

myMap2 f = foldl (\acc x -> acc ++ [f x]) []


-- ### Esercizio 2.4
-- TODO: da fare


-- ### Esercizio 3.1
-- O(n^3)
prefissi1 [] = []
prefissi1 xs = (prefissi1 $ reverse $ tail $ reverse xs) ++ [xs]

-- O(n^2)
prefissi2Internals _ [] = []
prefissi2Internals ps (x:xs) = pref : prefissi2Internals pref xs
    where pref = ps ++ [x]

prefissi2 xs = prefissi2Internals [] xs


-- ### Esercizio 3.2
segSommaS [x] s = if x == s then [[x]] else []
segSommaS xs@(_:txs) s = filter (\l -> s == sum l) (prefissi2 xs) ++ segSommaS txs s


-- ### Esercizio 3.3
-- TODO: da fare


-- ### Esercizio 4.1
-- O(n^3)
partInternals 0 _ _ = 1
partInternals n j k
    | j == k = 1
    | j < k = 0
    | j > k = sum [partInternals n (j - k) i | i <- [k..n]]

part n = partInternals n (n + 1) 1


-- ### Esercizio 4.2
-- O(2^n)
part2 n = if n <= 0 then 1 else sum [part2 (n - x) | x <- [1..n]]


-- ### Esercizio 4.3
-- O(2^n)
-- TODO FILTRALE
partsInternals l n = if n <= 0 then [l] else concat [partsInternals (l ++ [x]) (n - x) | x <- [1..n]]

parts n = partsInternals [] n


-- ### Esercizio 4.4
-- TODO: giustificare complessità
part3 n = length $ parts n


main :: IO ()
-- main = do putStrLn $ show $ segSommaS [4, 2, 3, 4] 9
-- main = do putStrLn $ show $ part2 3
-- main = do putStrLn $ show $ myMap2 (+3) [1, 2, 3]
-- main = do putStrLn $ show $ myRemoveDupsOrd $ sort [5, 2, 1, 2, 5, 7, 2, 1, 2, 7]
main = do putStrLn $ show $ part 16
