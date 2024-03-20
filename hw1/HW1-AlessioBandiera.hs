import Data.List (sort)

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
enumerateRev' _ [] = []
enumerateRev' n [x] = [(x, n)]
enumerateRev' n (x:xs) = (x, n) : enumerateRev' (n + 1) xs

enumerateRev xs = enumerateRev' 0 xs

myRemoveDups l = map (\x -> snd x) (fst zippedHead : snd zippedHead : map snd (tail zipped))
    where exs = enumerate Rev l
          sxs = sort $ exs
          zipped = filter (uncurry $ \x y -> fst x /= fst y) (zip sxs (tail sxs))
          zippedHead = head zipped


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
-- Non è possibile definire foldl/foldr utilizzando map in quanto
-- il tipo della funzione che map prende in input è a -> b, mentre
-- il tipo della funzione che foldl e foldr si aspettano in input
-- sono rispettivamente b -> a -> b e a -> b -> b, che sono molto
-- più generali (poiché devono permettere di gestire un accumulatore,
-- cosa di cui invece map non dispone)


-- ### Esercizio 3.1
-- O(n^2)
prefissi' _ [] = []
prefissi' ps (x:xs) = pref : prefissi' pref xs
    where pref = ps ++ [x]

prefissi xs = prefissi' [] xs


-- ### Esercizio 3.2
suffissi xs@(_:txs) = xs : suffissi txs
suffissi [] = []

-- TODO: forse rifallo?
segSommaS [x] s = if x == s then [[x]] else []
segSommaS xs@(_:txs) s = filter (\l -> s == sum l) (prefissi xs) ++ segSommaS txs s


-- ### Esercizio 3.3
-- O(2^n)
sublSommaS' [x] = [[x]]
sublSommaS' (x:xs) = [[x]] ++ [[x] ++ r | r <- rest] ++ rest
    where rest = sublSommaS'(xs)

sublSommaS l n = filter (\x -> sum x == n) (sublSommaS' l)


-- ### Esercizio 4.1
-- O(n^3) TODO: MISA CHE È SBAGLIATO, CONSIDERA DI RIFARLO COME IERI SERA
part' 0 _ _ = 1
part' n j k
    | j == k = 1
    | j < k = 0
    | j > k = sum [part' n (j - k) i | i <- [k..n]]

part n = part' n (n + 1) 1


-- ### Esercizio 4.2
-- O(2^n)
part2 0 = 1
part2 n = sum [part2 (n - x) | x <- [1..n]]


-- ### Esercizio 4.3
-- O(2^n)
-- TODO: RIFALLO
-- parts' l n = if n <= 0 then [l] else concat [parts' (l ++ [x]) (n - x) | x <- [1..n]]
--
-- parts n = parts' [] n
-- parts [x] = [x]
-- parts (x:y:xs) = 


-- ### Esercizio 4.4
-- TODO: giustificare complessità
-- part3 n = length $ parts n


main :: IO ()
-- main = do putStrLn $ show $ sublSommaS [1, 2, 1, 2, 5, 3, 2, 4] 4
main = do putStrLn $ show $ myRemoveDups [5, 2, 1, 2, 5, 7, 2, 1, 2, 7]
-- main = do putStrLn $ show $ segSommaS [4, 2, 3, 4] 9
-- main = do putStrLn $ show $ part2 1
-- main = do putStrLn $ show $ myMap2 (+3) [1, 2, 3]
-- main = do putStrLn $ show $ myRemoveDupsOrd $ sort [5, 2, 1, 2, 5, 7, 2, 1, 2, 7]
-- main = do putStrLn $ show $ parts [1, 1, 1, 1]
