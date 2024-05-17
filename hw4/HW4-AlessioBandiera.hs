import Control.Monad.State

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
f''' (x:xs) = pure (:) <*> (pure (*) <*> get <*> pure x ) <*> (state (\s -> (id, s + 1)) <*> f''' xs)


balancedNodes Empty = return []
balancedNodes (Node a sx dx) = do (path, subtree) <- get
                                  put (path + a, subtree)
                                  bsx <- balancedNodes sx
                                  (pathSx, subtreeSx) <- get
                                  put (path + a, subtree)
                                  bdx <- balancedNodes dx
                                  (pathDx, subtreeDx) <- get
                                  put (path + a, subtreeSx + subtreeDx + a)
                                  return (if path == subtreeDx + subtreeSx + a then a:bsx ++ bdx else bsx ++ bdx)


-- ### Esercizio 3
data NatBin = Zero NatBin | One NatBin | End
    deriving (Eq, Ord, Show)

data NatBinExp = Const NatBin | Div NatBinExp NatBinExp 
    deriving Show

data NatBinExpErr = ZeroDivision
    deriving Show

-- Add Term Term | Sub Term Term | Mul Term Term | Div Term Term | Mod Term Term

fromNatBin :: NatBin -> Int
fromNatBin x = fromNatBinAux x 0
    where
        fromNatBinAux (Zero x) i = fromNatBinAux x (i + 1)
        fromNatBinAux (One x) i = 1 * (2^i) + fromNatBinAux x (i + 1)
        fromNatBinAux End _ = 0

intoNatBin :: Int -> NatBin
intoNatBin n = intoNatBinAux (n `divMod` 2)
    where
        intoNatBinAux (0, 0) = End
        intoNatBinAux (n, 0) = Zero (intoNatBinAux (n `divMod` 2))
        intoNatBinAux (n, 1) = One (intoNatBinAux (n `divMod` 2))

safeDivNatBin :: Maybe NatBin -> Maybe NatBin -> Maybe NatBin
safeDivNatBin u v = do a <- u
                       b <- v
                       if (fromNatBin b) /= 0 then pure (intoNatBin ((fromNatBin a) `div` (fromNatBin b))) else Nothing

logNatBin :: Maybe NatBin -> NatBinExp -> State (Maybe NatBinExpErr) (Maybe NatBin)
logNatBin Nothing (Div x y) = state (\s -> (Nothing, Just ZeroDivision))
logNatBin v _ = state (\s -> (v, s))

eval :: NatBinExp -> State (Maybe NatBinExpErr) (Maybe NatBin)
eval (Const x) = state (\s -> (Just x, s))
eval (Div x y) = do u <- eval x
                    v <- eval y
                    logNatBin (safeDivNatBin u v) (Div x y)

main :: IO ()
-- main = do putStrLn $ show $ "Alessio Bandiera"
-- main = do putStrLn $ show $ runState (balancedNodes (Node 1 (Node 7 (Node 5 (Node 1 Empty Empty) (Node 1 Empty (Node 1 Empty Empty))) Empty) (Node 3 (Node 2 (Node 1 Empty Empty) (Node 1 Empty Empty)) Empty))) (0, 0)
-- main = do putStrLn $ show $ let term = Div (Const (intoNatBin 6)) (Const (intoNatBin 3)) in runState (eval term) Nothing
-- main = do putStrLn $ show $ let term = Div (Const (intoNatBin 6)) (Const (intoNatBin 0)) in runState (eval term) Nothing
main = do putStrLn $ show $ runState (f''' [1, 1, 1]) 1
