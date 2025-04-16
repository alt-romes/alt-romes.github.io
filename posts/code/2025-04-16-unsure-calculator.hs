{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Text.Printf
import qualified Data.Map as M
import Data.Maybe

newtype Dist a = Dist { unpackDist :: [(a, Double)] }
  deriving Functor

instance Applicative Dist where
  pure x = Dist [(x, 1.0)]
  (Dist fs) <*> (Dist xs) = Dist $ do
    (x, px) <- xs
    (f, pf) <- fs
    return (f x, px * pf)

instance Monad Dist where
  (Dist xs) >>= f = Dist $ do
    (x, p) <- xs
    (y, p') <- unpackDist (f x)
    return (y, p * p')

squishD (Dist xs) = Dist $ M.toList $ M.fromListWith (+) xs
sumP = sum . map snd
normP xs = [(x, p / q) | let q = sumP xs, (x, p) <- xs]

instance (Ord a, Show a, PrintfArg a) => Show (Dist a) where
  show d = concatMap showRow $ (normP . unpackDist . squishD) d
    where
      -- showRow (elem, prob) = printf "%.2f" elem ++ " | " ++ printf "%.2f" (prob*100) ++ "%\n"
      showRow (elem, prob) = padded elem ++ " | " ++ printf "%.2f" (prob*100) ++ "%\n"
      padded elem = replicate (maxElemLen - (length . show) elem) ' ' ++ show elem
      maxElemLen = maximum $ map (length . show . fst) (unpackDist d)

-- -- put in 20 buckets
buckets :: Dist Double -> Dist Double
buckets (Dist [])   = Dist []
buckets d@(Dist xs) = Dist $ catMaybes $ fmap interval xs where
  low  = fst $ head xs
  high = fst $ last xs
  step = (high - low)/20
  steps = [low, low+step .. high]
  interval (x, p)
    = case takeWhile (\b -> b <= x) steps of
        [] -> Nothing
        (i:_) -> Just (i, p)


uniform xs = Dist . normP $ zip xs (repeat 1)
coin f a b
  | f < 0 || f > 1 = error "f must be between 0 and 1"
  | otherwise      = Dist [(a, f), (b, 1 - f)]

data Expr
  = Num Double | Add Expr Expr | Mul Expr Expr
  | Abs Expr | Signum Expr | Negate Expr
  | Div Expr Expr
  | Exp Expr | Log Expr
  | Sin Expr | Cos Expr
  | Range Expr Expr

(~) = Range

instance Num Expr where
  (+) = Add
  (*) = Mul
  abs = Abs
  signum = Signum
  fromInteger = Num . fromInteger
  negate = Negate

instance Fractional Expr where
  (/) = Div
  fromRational = Num . fromRational

instance Floating Expr where
  pi = Num pi
  exp = Exp
  log = Log
  sin = Sin
  cos = Cos

instance Show Expr where
  show = show . buckets . eval

eval :: Expr -> Dist Double
eval = \case
  Num d       -> return d
  Add e1 e2   -> (+) <$> eval e1 <*> eval e2
  Mul e1 e2   -> (*) <$> eval e1 <*> eval e2
  Abs e       -> abs <$> eval e
  Signum e    -> signum <$> eval e
  Negate e    -> negate <$> eval e
  Div e1 e2   -> (/) <$> eval e1 <*> eval e2
  Exp e       -> exp <$> eval e
  Log e       -> log <$> eval e
  Sin e       -> sin <$> eval e
  Cos e       -> cos <$> eval e
  Range e1 e2 -> do
    a <- eval e1
    b <- eval e2

    -- μ - 2σ = a
    -- μ + 2σ = b
    -- μ = b - 2σ = (b + a) / 2
    -- σ = (μ - a)/2
    -- σ = (b - 2σ - a)/2
    -- σ = b/2 - σ - a/2
    -- σ  = (b - a)/4
    -- μ = b - (b - a)/2 = b - b/2 + a/2 = b/2 + a/2 = (b + a)/2

    let
      samples = [a, a+0.5 .. b]
      mean = (a + b) / 2
      std_dev = (b - a) / 4
      normal_f x = (1/(std_dev*sqrt(2*pi))) * exp ((-1/2)*(((x-mean)/std_dev)**2))

    Dist $ map (\x -> (x, normal_f x)) samples

