import Control.Monad.State
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Function
import Data.List (find)

-- ### Esercizio 2
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show

fresh1 val = state (\(n1, n2) -> (n1, (n1 + val, n2)))
append val = state (\(n1, n2) -> (n1, (n1, val : n2)))
updateSub val = state (\(n1, n2) -> (val + sum (take 2 n2), (n1 - val, val + sum (take 2 n2) : (drop 2 n2))))

balancedNodesM b = evalState (balancedNodesMAux b) (0, [])
    where
        balancedNodesMAux Empty = do _ <- append 0
                                     return []
        balancedNodesMAux (Node val left right) = do n <- fresh1 val
                                                     lres <- balancedNodesMAux left
                                                     rres <- balancedNodesMAux right
                                                     totSubSum <- updateSub val
                                                     return ((if n == totSubSum then (val:) else id) lres ++ rres)

balancedNodesA b = evalState (balancedNodesAAux b) (0, [])
    where
        balancedNodesAAux Empty = append 0 *> pure []
        balancedNodesAAux (Node val left right) = (\n lres rres totSubSum -> (if n == totSubSum then (val:) else id) lres ++ rres)
                                                  <$> fresh1 val
                                                  <*> balancedNodesAAux left
                                                  <*> balancedNodesAAux right
                                                  <*> updateSub val

-- ### Esercizio 3
data NatBin = End | Zero NatBin | One NatBin
    deriving Show

instance Eq NatBin where
    m == n = equalAux (removeLeadingZeros m) (removeLeadingZeros n)
        where
            equalAux End End = True
            equalAux End _ = False
            equalAux _ End = False
            equalAux (Zero x) (Zero y) = equalAux x y
            equalAux (Zero x) (One y) = False
            equalAux (One x) (Zero y) = False
            equalAux (One x) (One y) = equalAux x y

instance Ord NatBin where
    m <= n = let (c, t) = compareAux (removeLeadingZeros m) (removeLeadingZeros n) in if t then c else True
        where
            compareAux End End = (True, False)
            compareAux _ End = (False, True)
            compareAux End _ = (True, True)
            compareAux (Zero x) (Zero y) = compareAux x y
            compareAux (Zero x) (One y) = let (c, t) = compareAux x y in if t then (c, t) else (True, True)
            compareAux (One x) (Zero y) = let (c, t) = compareAux x y in if t then (c, t) else (False, True)
            compareAux (One x) (One y) = compareAux x y

isByte x = (x <= 255) && (x >= 0)

fromNatBin :: NatBin -> Maybe Int
fromNatBin x = let n = fromNatBinAux x 0 in if isByte n then Just n else Nothing
    where
        fromNatBinAux (Zero x) i = fromNatBinAux x (i + 1)
        fromNatBinAux (One x) i = 1 * (2^i) + fromNatBinAux x (i + 1)
        fromNatBinAux End _ = 0

intoNatBin :: Int -> Maybe NatBin
intoNatBin 0 = Just (Zero End)
intoNatBin n = if isByte n then Just (intoNatBinAux (n `divMod` 2)) else Nothing
    where
        intoNatBinAux (0, 0) = End
        intoNatBinAux (n, 0) = Zero (intoNatBinAux (n `divMod` 2))
        intoNatBinAux (n, 1) = One (intoNatBinAux (n `divMod` 2))

removeLeadingZeros n = fst (removeLeadingZerosAux n 0 0)
    where
        removeLeadingZerosAux End c l = if l == c then (End, c - 1) else (End, c)
        removeLeadingZerosAux (Zero x) c l = let (res, c') = removeLeadingZerosAux x (c + 1) (l + 1) in if c' /= 0 then (res, c' - 1) else (Zero res, c')
        removeLeadingZerosAux (One x) c l = let (res, c') = removeLeadingZerosAux x 0 (l + 1) in (One res, 0)

zeroNatBin = Zero End
oneNatBin = One End
minValueNatBin = zeroNatBin
maxValueNatBin = fromJust (intoNatBin 255)

lengthNatBin m = lengthNatBinAux (removeLeadingZeros m)
    where
        lengthNatBinAux End = 0
        lengthNatBinAux (Zero x) = 1 + lengthNatBinAux x
        lengthNatBinAux (One x) = 1 + lengthNatBinAux x

isByteNatBin n = n <= maxValueNatBin

isEndNatBin n = n == End

data Term = Value NatBin | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term | Mod Term Term
    deriving Show

data MaybeTerm a = JustTerm a | ZeroDivisionErr | NegativeNumberErr | OverflowErr | InvalidNatBinErr
    deriving Show

instance Functor MaybeTerm where
    fmap f ZeroDivisionErr = ZeroDivisionErr
    fmap f NegativeNumberErr = NegativeNumberErr
    fmap f OverflowErr = OverflowErr
    fmap f InvalidNatBinErr = InvalidNatBinErr
    fmap f (JustTerm x) = JustTerm (f x)

instance Applicative MaybeTerm where
    pure = JustTerm
    ZeroDivisionErr <*> _ = ZeroDivisionErr
    NegativeNumberErr <*> _ = NegativeNumberErr
    OverflowErr <*> _ = OverflowErr
    InvalidNatBinErr <*> _ = InvalidNatBinErr
    (JustTerm f) <*> mx = fmap f mx

instance Monad MaybeTerm where
    return = pure
    ZeroDivisionErr >>= _ = ZeroDivisionErr
    NegativeNumberErr >>= _ = NegativeNumberErr
    OverflowErr >>= _ = OverflowErr
    InvalidNatBinErr >>= _ = InvalidNatBinErr
    (JustTerm x) >>= f = f x

fromJustTerm ZeroDivisionErr = error "MaybeTerm.fromJustTerm: ZeroDivisionErr"
fromJustTerm NegativeNumberErr = error "MaybeTerm.fromJustTerm: NegativeNumberErr"
fromJustTerm OverflowErr = error "MaybeTerm.fromJustTerm: OverflowErr"
fromJustTerm InvalidNatBinErr = error "MaybeTerm.fromJustTerm: InvalidNatBinErr"
fromJustTerm (JustTerm x) = x

checkNatBinValue x
    | isEndNatBin x = InvalidNatBinErr
    | not (isByteNatBin x) = OverflowErr
    | otherwise = JustTerm (removeLeadingZeros x)

addNatBin m n = let res = addNatBinAux m n 0 in if isByteNatBin res then JustTerm res else OverflowErr
    where
        addNatBinAux End End 0 = End
        addNatBinAux End End 1 = One End
        addNatBinAux (Zero x) End 0 = Zero (addNatBinAux x End 0)
        addNatBinAux (Zero x) End 1 = One (addNatBinAux x End 0)
        addNatBinAux (One x) End 0 = One (addNatBinAux x End 0)
        addNatBinAux (One x) End 1 = Zero (addNatBinAux x End 1)
        addNatBinAux End (Zero x) 0 = Zero (addNatBinAux End x 0)
        addNatBinAux End (Zero x) 1 = One (addNatBinAux End x 0)
        addNatBinAux End (One x) 0 = One (addNatBinAux End x 0)
        addNatBinAux End (One x) 1 = Zero (addNatBinAux End x 1)
        addNatBinAux (Zero x) (Zero y) 0 = Zero (addNatBinAux x y 0)
        addNatBinAux (Zero x) (Zero y) 1 = One (addNatBinAux x y 0)
        addNatBinAux (Zero x) (One y) 0 = One (addNatBinAux x y 0)
        addNatBinAux (Zero x) (One y) 1 = Zero (addNatBinAux x y 1)
        addNatBinAux (One x) (Zero y) 0 = One (addNatBinAux x y 0)
        addNatBinAux (One x) (Zero y) 1 = Zero (addNatBinAux x y 1)
        addNatBinAux (One x) (One y) 0 = Zero (addNatBinAux x y 1)
        addNatBinAux (One x) (One y) 1 = One (addNatBinAux x y 1)

subNatBin m n = if m >= n then JustTerm (subNatBinAux m n 0) else NegativeNumberErr
    where
        subNatBinAux End End _ = End
        subNatBinAux (Zero x) End 0 = Zero (subNatBinAux x End 0)
        subNatBinAux (Zero x) End 1 = One (subNatBinAux x End 1)
        subNatBinAux (One x) End 0 = One (subNatBinAux x End 0)
        subNatBinAux (One x) End 1 = Zero (subNatBinAux x End 0)
        subNatBinAux End (Zero x) 0 = Zero (subNatBinAux End x 0)
        subNatBinAux End (Zero x) 1 = One (subNatBinAux End x 1)
        subNatBinAux End (One x) 0 = One (subNatBinAux End x 1)
        subNatBinAux End (One x) 1 = Zero (subNatBinAux End x 1)
        subNatBinAux (Zero x) (Zero y) 0 = Zero (subNatBinAux x y 0)
        subNatBinAux (Zero x) (Zero y) 1 = One (subNatBinAux x y 1)
        subNatBinAux (Zero x) (One y) 0 = One (subNatBinAux x y 1)
        subNatBinAux (Zero x) (One y) 1 = Zero (subNatBinAux x y 1)
        subNatBinAux (One x) (Zero y) 0 = One (subNatBinAux x y 0)
        subNatBinAux (One x) (Zero y) 1 = Zero (subNatBinAux x y 0)
        subNatBinAux (One x) (One y) 0 = Zero (subNatBinAux x y 0)
        subNatBinAux (One x) (One y) 1 = One (subNatBinAux x y 1)

mulNatBin m n = mulNatBinAux m n
    where
        mulNatBinAux m n
            | n == zeroNatBin = JustTerm zeroNatBin
            | otherwise = do nMinusOne <- n `subNatBin` oneNatBin
                             res <- mulNatBinAux m nMinusOne
                             m `addNatBin` res

divModNatBin m n = if n /= zeroNatBin then divModNatBinAux m n zeroNatBin else ZeroDivisionErr
    where
        divModNatBinAux m n q
            | m < n = JustTerm (q, m)
            | otherwise = do mMinusn <- m `subNatBin` n
                             qPlusOne <- q `addNatBin` oneNatBin
                             divModNatBinAux mMinusn n qPlusOne

evalTerm :: Term -> MaybeTerm NatBin
evalTerm (Value x) = checkNatBinValue x
evalTerm (Add x y) = do m <- evalTerm x
                        n <- evalTerm y
                        m `addNatBin` n
evalTerm (Sub x y) = do m <- evalTerm x
                        n <- evalTerm y
                        m `subNatBin` n
evalTerm (Mul x y) = do m <- evalTerm x
                        n <- evalTerm y
                        m `mulNatBin` n
evalTerm (Div x y) = do m <- evalTerm x
                        n <- evalTerm y
                        (q, _) <- m `divModNatBin` n
                        return q
evalTerm (Mod x y) = do m <- evalTerm x
                        n <- evalTerm y
                        (_, r) <- m `divModNatBin` n
                        return r

main :: IO ()
-- main = do putStrLn $ show $ "Alessio Bandiera"
-- main = do putStrLn $ show $ [5, 2] == balancedNodesM (Node 1 (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty (Node 1 Empty Empty))) Empty) (Node 3 (Node 2 (Node 1 Empty Empty) (Node 1 Empty Empty)) Empty))
-- main = do putStrLn $ show $ and [x + y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Add (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [0..255], x + y <= 255]
-- main = do putStrLn $ show $ and [x - y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Sub (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [0..255], x >= y]
-- main = do putStrLn $ show $ and [x * y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Mul (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [0..255], x * y <= 255]
-- main = do putStrLn $ show $ and [x `div` y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Div (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [1..255]]
main = do putStrLn $ show $ and [x `mod` y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Mod (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [1..255]]
-- main = do putStrLn $ show $ takeWhile (\(x, y, d, dn) -> d == dn) [(x, y, x `div` y, fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Div (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..255], y <- [1..255]]
-- main = do putStrLn $ show $ [(x, y, fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Div (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..2], y <- [1..2]]
-- main = do putStrLn $ show $ (==) (One (Zero End)) (One End)
-- main = do putStrLn $ show $ and [(x <= y) == ((fromJust $ intoNatBin x) <= (fromJust $ intoNatBin y)) | x <- [0..128], y <- [0..127]]
-- main = do putStrLn $ show $ and [(x == y) == ((fromJust $ intoNatBin x) == (fromJust $ intoNatBin y)) | x <- [0..128], y <- [0..127]]
-- main = do putStrLn $ show $ find (\(_, _, c) -> c == False) [(x, y, (x <= y) == ((fromJust $ intoNatBin x) <= (fromJust $ intoNatBin y))) | x <- [0..128], y <- [0..127]]
-- main = do putStrLn $ show $ find (\(_, _, c) -> c == False) [(x, y, (x == y) == ((fromJust $ intoNatBin x) == (fromJust $ intoNatBin y))) | x <- [0..128], y <- [0..127]]
-- main = do putStrLn $ show $ removeLeadingZeros $ Zero $ Zero $ Zero $ Zero End
-- main = do putStrLn $ show $ evalTerm (Value $ One $ One $ One $ One $ One $ One $ One $ One $ One End )
-- main = do putStrLn $ show $ removeLeadingZeros $ Zero $ One $ Zero $ Zero $ One $ One $ Zero $ Zero End
-- main = do putStrLn $ show $ runState (f''' [1, 1, 1]) 1
