newtype ST s a = S (s -> (a, s))

app :: ST s a -> s -> (a, s)
app (S f) = f

instance Functor (ST s) where
    fmap f st = S (\s -> let (x, s') = app st s in (f x, s'))

instance Applicative (ST s) where
    pure x = S (\s -> (x, s))
    stf <*> stx = S (\s -> let (f, s') = app stf s in let (x, s'') = app stx s' in (f x, s''))

instance Monad (ST s) where
    stx >>= f = S (\s -> let (x, s') = app stx s in app (f x) s')
    return = pure

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

logNatBin :: Maybe NatBin -> NatBinExp -> ST (Maybe NatBinExpErr) (Maybe NatBin)
logNatBin Nothing (Div x y) = S (\s -> (Nothing, Just ZeroDivision))
logNatBin v _ = S (\s -> (v, s))

eval :: NatBinExp -> ST (Maybe NatBinExpErr) (Maybe NatBin)
eval (Const x) = S (\s -> (Just x, s))
eval (Div x y) = do u <- eval x
                    v <- eval y
                    logNatBin (safeDivNatBin u v) (Div x y)

main :: IO ()
-- main = do putStrLn $ show $ "Alessio Bandiera"
main = do putStrLn $ show $ let term = Div (Const (intoNatBin 6)) (Const (intoNatBin 3)) in app (eval term) Nothing
-- main = do putStrLn $ show $ let term = Div (Const (intoNatBin 6)) (Const (intoNatBin 0)) in app (eval term) Nothing
