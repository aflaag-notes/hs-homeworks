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

-- TODO: da controllare
myZipWith1 f xs ys = zapp (repeat (\t -> f (fst t) (snd t))) (zip xs ys)


-- ### Esercizio 2.2
-- TODO: da fare


-- ### Esercizio 2.3
myMap1 f = foldr ((:) . f) []

-- TODO: da fare


-- ### Esercizio 2.4
-- TODO: è per il tipo ma non so giustificarlo a sufficienza


-- ### Esercizio 3.1
-- O(n^2)
prefissi1 [] = []
prefissi1 xs = (prefissi1 $ reverse $ tail $ reverse xs) ++ [xs]

-- O(n)
prefissi2Internals _ [] = []
prefissi2Internals ss (x:xs) = suff : prefissi2Internals suff xs
    where suff = ss ++ [x]

prefissi2 xs = prefissi2Internals [] xs


-- ### Esercizio 3.2
segSommaS xs s = 

main :: IO ()
main = do putStrLn $ show $ segSommaS [4, 2, 3, 4] 9
