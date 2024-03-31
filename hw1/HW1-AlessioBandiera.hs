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
    | zxs == [] = []
    | otherwise = fst zh : snd zh : map snd (tail zxs)
    where zxs = filter (uncurry $ \x y -> x /= y) (zip xs $ tail xs)
          zh = head zxs


-- ### Esercizio 1.3
-- O(n)
filterdups [] = []
filterDups [x, y] = if fst x /= fst y then [x, y] else [y]
filterDups (x:y:xs) = if fst x /= fst y then x : filterDups (y:xs) else filterDups (y:xs)

-- O(n log n)
myRemoveDups xs = map snd $ sort $ map (\(x, y) -> (y, x)) (filterDups . reverse . sort $ zip xs [0..])


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
-- cosa di cui invece map non dispone).


-- ### Esercizio 3.1
-- O(n^2)
prefissi' _ [] = []
prefissi' ps (x:xs) = pref : prefissi' pref xs
    where pref = ps ++ [x]

prefissi xs = prefissi' [] xs


-- ### Esercizio 3.2
suffissi xs@(_:txs) = xs : suffissi txs
suffissi [] = []

segSommaS xs n = filter (\ys -> sum ys == n) (concat $ map suffissi $ prefissi xs)


-- ### Esercizio 3.3
-- O(2^n)
sublSommaS' [] = [[]]
sublSommaS' [x] = [[x]]
sublSommaS' (x:xs) = [[x]] ++ [[x] ++ r | r <- rest] ++ rest
    where rest = sublSommaS' xs

sublSommaS l n = filter (\ys -> sum ys == n) (sublSommaS' l)


-- ### Esercizio 4.1
-- O(2^n)
part' 0 _ = 1
part' n prev = sum [part' (n - x) x | x <- [1..n], x <= prev]

part n = part' n n


-- ### Esercizio 4.2
-- O(2^n)
part2 0 = 1
part2 n = sum [part2 (n - x) | x <- [1..n]]


-- ### Esercizio 4.3
-- O(2^n)
parts 0 = [[]]
parts 1 = [[1]]
parts n = [x : y | x <- [1..n], y <- parts (n - x), null y || head y <= x]


-- ### Esercizio 4.4
-- Il costo computazionale è sempre O(2^n) nonostante la valutazione lazy,
-- in quanto length deve comunque arrivare ai casi base, e anche se non
-- è necessario creare le liste, serve comunque sapere quanti elementi
-- queste devono contenere.
part3 n = length $ parts n

main :: IO ()
main = do putStrLn $ show $ part 4
