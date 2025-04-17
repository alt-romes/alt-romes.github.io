{-# LANGUAGE LambdaCase, ViewPatterns #-}
import Control.Applicative
import Text.Printf
import qualified Data.Map as M
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.Maybe
import Data.List (transpose, sort)

newtype Dist a = Dist { unpackDist :: [(a, Double)] } deriving Functor

instance Applicative Dist where
  pure x = Dist [(x, 1.0)]
  (Dist fs) <*> (Dist xs) = Dist $ do
    (x, px) <- xs
    (f, pf) <- fs
    let !fr = f x
    let !pr = px * pf
    return (fr, pr)

instance Monad Dist where
  (Dist xs) >>= f = Dist $ do
    (x, p) <- xs
    (y, p') <- unpackDist (f x)
    let !pr = p * p'
    return (y, pr)

squishD (Dist xs) = Dist $ M.toList $ M.fromListWith (+) xs
sumP = sum . map snd
normP xs = [(x, p / q) | let q = sumP xs, (x, p) <- xs]

instance (Ord a, Show a, PrintfArg a) => Show (Dist a) where
  show d = concatMap showRow xs
    where
      xs = (normP . unpackDist . squishD) d
      max_prob = snd $ maximumBy (compare `on` snd) xs :: Double
      showRow (elem, prob) = drawline max_prob prob ++ " | " ++ printf "%.1f (%.1f" elem (prob*100) ++ "%)\n"

-- uniform xs = Dist . normP $ zip xs (repeat 1)

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
  show = show . collapseIntervals 23 . eval

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

    -- Equations:
    -- μ - 2σ = a
    -- μ + 2σ = b
    -- Solutions:
    -- σ  = (b - a)/4
    -- μ = b - (b - a)/2 = b - b/2 + a/2 = b/2 + a/2 = (b + a)/2

    let
      mean = (a + b) / 2
      std_dev = (b - a) / 4
      step = std_dev / 5

    intervals [a-step, a .. b+step] $ normal mean std_dev

drawline :: Double -> Double -> String
drawline max n = replicate spaces ' ' ++ replicate normalized ':' where
  spaces = 35 - normalized
  normalized = (round ((n/max) * 30))

-- | Draw from a normal distribution
normal :: Double {-^ Mean -} -> Double {-^ Std dev -} -> Dist Double
-- generate a handful of discrete samples from this distribution using the Box-Muller transform
normal mean std_dev = Dist $ normP $
  filter (not . isInfinite . fst) $
    map (\z -> (z*std_dev + mean, 1)) $ boxMullers $ take 5000 $ interleave (halton 2) (halton 3)
  -- "probability=1" for every sample because samples are already distributed
  -- according to the probability function (s.t. grouping them into intervals
  -- will already yield higher probability for the intervals near the mean)

intervals :: [Double]    -- ^ Each double is a box where all floats less than this number fall, in order (list must be sorted low-to-high).
          -> Dist Double -- ^ A distribution with arbitrary doubles
          -> Dist Double -- ^ A distribution where all doubles fall into the given discrete categories
intervals boxes dist = squishD $ do
  s <- dist
  return $ fst $ minimumBy (compare `on` snd) $
    map (\box -> (box, abs (box - s))) $ filter (not . isInfinite) boxes

-- | Simplify samples into N intervals
collapseIntervals :: Int -> Dist Double -> Dist Double
collapseIntervals n d@(Dist xs) =
  let (low,_)  = minimumBy (compare `on` fst) xs
      (high,_) = maximumBy (compare `on` fst) xs
      step = (high - low) / fromIntegral n
   in intervals [low, low+step .. high] d

-- pkg: normaldistribution
--
-- Normal distribution approximation
-- ---------------------------------
-- | Box-Muller method for generating two normally distributed
-- independent random values from two uniformly distributed
-- independent random values.
boxMuller :: Floating a => a -> a -> (a,a)
boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

-- | Convert a list of uniformly distributed random values into a
-- list of normally distributed random values. The Box-Muller
-- algorithms converts values two at a time, so if the input list
-- has an uneven number of element the last one will be discarded.
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
boxMullers _          = []

-- Halton sequence, a low discrepancy sequence to feed boxMullers
-- https://en.wikipedia.org/wiki/Low-discrepancy_sequence
halton :: Int {-^ Base -} -> [Double]
halton b = map (go 1 0) [1..] where
  go :: Double -> Double -> Int -> Double
  go f r i
    | i > 0
    , let f' = f / (fromIntegral b)
    , let r' = r + f'*(fromIntegral $ i `mod` b)
    , let i' = floor (fromIntegral i / fromIntegral b)
    = go f' r' i'
    | otherwise
    = r

interleave xs ys = concat (transpose [xs, ys])
