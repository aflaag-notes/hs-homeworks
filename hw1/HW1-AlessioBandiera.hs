-- ### Esercizio 1.1
myTakeWhile p [] = []
myTakeWhile p (x:xs) = x : if p x then myTakeWhile p xs else []

-- ### Esercizio 1.2
myDropWhile p [] = []
myDropWhile p (x:xs) = if p x then myDropWhile p xs else x:xs

-- ### Esercizio 1.3
-- TODO: da fare

-- ### Esercizio 2.1
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []

-- TODO: da controllare
myZipWith1 f xs ys = zapp (repeat (\t -> f (fst t) (snd t))) (zip xs ys)

-- ### Esercizio 2.2
-- TODO: da controllare
myZipWith2 f (x:xs) (y:ys) = f x y : myZipWith2 f xs ys
myZipWith2 _ _ _ = []

main :: IO ()
main = do putStrLn $ show $ myZipWith2 (+) [1, 2, 3] [4, 5]
