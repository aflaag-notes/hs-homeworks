import Control.Monad.State
import Data.Maybe (fromJust)
import Control.Applicative
import Data.Function

-- ### Esercizio 2
data BinTree a = Node a (BinTree a) (BinTree a) | Empty
    deriving Show

-- pure x = S (\s -> (x, s))
-- stf <*> stx = S (\s -> let (f, s') = runState stf s in let (x, s'') = runState stx s' in (f x, s''))

-- m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

f :: (Num a) => [a] -> State a [a]
f [] = return []
f (x:xs) = do res <- f xs
              y <- get
              put (y + 1)
              return (y * x : res)

f' [] = state (\s -> ([], s))
f' (x:xs) = f' xs >>= (\res -> get >>= (\y -> put (y + 1) >> (state (\s -> (y * x : res, s)))))

f'' [] = state (\s -> ([], s))
f'' (x:xs) = state (\s -> let (res, s') = runState (f'' xs) s
                          in runState (state (\t -> let (y, t') = runState (state (\v -> (v, v))) t
                                                    in runState (state (\v -> let (_, v') = runState (state (\z -> ((), z + 1))) v
                                                                              in runState (state (\u -> (y * x : res, u))) v')) t')) s')

f''' :: [Int] -> State Int [Int]
f''' [] = pure []
f''' (x:xs) = f''' xs <**> (pure (:) <*> (pure (*) <*> get <*> (state (\s -> (id, s + 1)) <*> pure x)))

balancedNodes b = evalState (balancedNodesAux b) (0, 0)
    where
        balancedNodesAux Empty = return []
        balancedNodesAux (Node a sx dx) = do (path, subtree) <- get
                                             let totPath = path + a
                                             put (totPath, subtree)
                                             bsx <- balancedNodesAux sx
                                             (pathSx, subtreeSx) <- get
                                             put (totPath, subtree)
                                             bdx <- balancedNodesAux dx
                                             (pathDx, subtreeDx) <- get
                                             let totSubtree = subtreeSx + subtreeDx + a
                                             let bs = bsx ++ bdx
                                             put (totPath, totSubtree)
                                             return (if path == totSubtree then a:bs else bs)

-- balancedNodes' Empty = pure []
-- balancedNodes' (Node a sx dx) = pure (+) <*> (state (\s -> let (_, v) = runState (state (\(path, subtree) -> (id, (path + a, subtree))) <*> balancedNodes' sx) s in (v, v))) <*> (pure (+) <*> pure a <*> ())
-- balancedNodes' (Node a sx dx) = (state (\_ -> (id, (path + a, subtree))) <*> balancedNodes' sx) <**> (state (\_ -> (path + a, subtree)) <*> balancedNodes' dx)
--     where
--         (path, subtree) = get
--         (pathSx, subtreeSx) = get
--         (pathDx, subtreeDx) = get


-- ### Esercizio 3
data NatBin = Zero NatBin | One NatBin | End
    deriving (Eq, Ord, Show)

removeLeadingZeros n = fst (removeLeadingZerosAux n 0 0)
    where
        removeLeadingZerosAux End c l = if l == c then (End, c - 1) else (End, c)
        removeLeadingZerosAux (Zero x) c l = let (res, c') = removeLeadingZerosAux x (c + 1) (l + 1) in if c' /= 0 then (res, c' - 1) else (Zero res, c')
        removeLeadingZerosAux (One x) c l = let (res, c') = removeLeadingZerosAux x 0 (l + 1) in (One res, 0)

lengthNatBin End = 0
lengthNatBin (Zero x) = 1 + lengthNatBin x
lengthNatBin (One x) = 1 + lengthNatBin x 

isByteNatBin n = lengthNatBin (removeLeadingZeros n) <= 8

isEndNatBin n = n == End

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

addNatBin m n = addNatBinAux m n 0
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

subNatBin m n = subNatBinAux m n 0
    where
        subNatBinAux (Zero x) (Zero y) 0 = Zero (subNatBinAux x y 0)
        subNatBinAux (Zero x) (Zero y) 0 = Zero (subNatBinAux x y 0)
        subNatBinAux (Zero x) (Zero y) 1 = Zero (subNatBinAux x y 1)
        subNatBinAux (Zero x) (One y) 0 = One (subNatBinAux x y 1)
        subNatBinAux (Zero x) (One y) 1 = One (subNatBinAux x y 1)
        subNatBinAux (One x) (Zero y) 0 = One (subNatBinAux x y 0)
        subNatBinAux (One x) (Zero y) 1 = Zero (subNatBinAux x y 0)
        subNatBinAux (One x) (One y) 0 = Zero (subNatBinAux x y 0)
        subNatBinAux (One x) (One y) 1 = One (subNatBinAux x y 1)

data Term = Value NatBin | Add Term Term | Sub Term Term -- | Mul Term Term | Div Term Term | Mod Term Term
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

evalTerm :: Term -> MaybeTerm NatBin
evalTerm (Value x) = checkNatBinValue x
evalTerm (Add x y) = do m <- evalTerm x
                        n <- evalTerm y
                        let res = m `addNatBin` n in if isByteNatBin res then JustTerm res else OverflowErr

main :: IO ()
-- main = do putStrLn $ show $ "Alessio Bandiera"
-- main = do putStrLn $ show $ [5, 2] == balancedNodes (Node 1 (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty (Node 1 Empty Empty))) Empty) (Node 3 (Node 2 (Node 1 Empty Empty) (Node 1 Empty Empty)) Empty))
main = do putStrLn $ show $ and [x + y == (fromJust $ fromNatBin $ fromJustTerm (evalTerm $ Add (Value $ fromJust $ intoNatBin x) (Value $ fromJust $ intoNatBin y))) | x <- [0..128], y <- [0..127]]
-- main = do putStrLn $ show $ removeLeadingZeros $ Zero $ Zero $ Zero $ Zero End
-- main = do putStrLn $ show $ evalTerm (Value $ One $ One $ One $ One $ One $ One $ One $ One $ One End )
-- main = do putStrLn $ show $ removeLeadingZeros $ Zero $ One $ Zero $ Zero $ One $ One $ Zero $ Zero End
-- main = do putStrLn $ show $ runState (f''' [1, 1, 1]) 1
