-- ### Esercizio 1.1
myTakeWhile p [] = []
myTakeWhile p (x:xs) = x : if p x then myTakeWhile p xs else []


-- ### Esercizio 1.2
myDropWhile p [] = []
myDropWhile p xs@(x:txs) = if p x then myDropWhile p txs else xs


-- ### Esercizio 1.3
-- TODO: da fare


-- ### Esercizio 2.1
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []

myZipWith1 f xs ys = zapp (map f xs) ys


-- ### Esercizio 2.2
myZipWith2 f xs ys = map (\t -> f (fst t) (snd t)) (zip xs ys)


-- ### Esercizio 2.3
myMap1 f = foldr ((:) . f) []

-- TODO: da fare


-- ### Esercizio 2.4
-- TODO: è per il tipo ma non so giustificarlo a sufficienza


-- ### Esercizio 3.1
-- O(n^3)
prefissi1 [] = []
prefissi1 xs = (prefissi1 $ reverse $ tail $ reverse xs) ++ [xs]

-- O(n^2)
prefissi2Internals _ [] = []
prefissi2Internals ss (x:xs) = suff : prefissi2Internals suff xs
    where suff = ss ++ [x]

prefissi2 xs = prefissi2Internals [] xs


-- ### Esercizio 3.2
suffissi xs@(_:txs) = xs : suffissi txs
suffissi [] = []

segSommaS [x] s = if x == s then [x] else []
segSommaS xs@(_:txs) s = (filter (\l -> s == sum l) (prefissi2 xs)) : segSommaS txs s


-- ### Esercizio 3.3



-- ## Esercizio 
-- part1Internals n x y M = if y < x then 
--
-- part1 n = part1Internals n 0 1 [[0 | _ <- take n [0..]] | _ <- take n [0..]]

-- part1 0 0 = 1
-- part1 0 _ = 0
-- part1 j k = if j < k then prev else prev + part1 (j - 1) (k - j)
--     where prev = part1 (j - 1) k

-- part1

-- ## Esercizio 4.2
part2 n = if n <= 0 then 1 else sum (map (\x -> part2 (n - x)) [1..n])

main :: IO ()
-- main = do putStrLn $ show $ segSommaS [4, 2, 3, 4] 9
-- main = do putStrLn $ show $ part2 4
main = do putStrLn $ show $ myZipWith2 (+) [1, 2, 3] [4, 5, 6]
