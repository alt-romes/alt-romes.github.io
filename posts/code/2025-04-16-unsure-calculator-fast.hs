{-# LANGUAGE LambdaCase, GADTs, ViewPatterns #-}
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.Functor.Const
import qualified Data.Map as M
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.Maybe
import Data.List (transpose, sort)
import Data.IORef
import System.IO.Unsafe


-- https://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf
data Dist a where
  Return :: a -> Dist a
  Bind :: (Dist b) -> (b -> Dist a) -> Dist a
  Primitive :: Sampleable d => d a -> Dist a
  -- Conditional :: (a -> Prob) -> Dist a -> Dist a

instance Functor Dist where
  fmap = liftM

instance Applicative Dist where
  pure = Return
  (<*>) = ap

instance Monad Dist where
  (>>=) = Bind

class Sampleable d where
  sample :: d a -> a

instance Sampleable Dist where
  sample = \case
    Return x -> x
    Primitive p -> sample p
    Bind d f -> sample . f $ sample d

data Normal a where
  Normal :: IO Double -> Normal Double

instance Sampleable Normal where
  sample (Normal s) = unsafePerformIO s

normal :: Double -> Double -> Dist Double
normal mean std_dev = Primitive $ Normal $ do
  s <- newNormalSample
  return $ s*std_dev + mean

normalSamples :: IORef [Double]
normalSamples = unsafePerformIO $ newIORef $
                  boxMullers $ interleave (halton 2) (halton 3)
{-# NOINLINE normalSamples #-}
newNormalSample :: IO Double
newNormalSample = do
  (s:ss) <- readIORef normalSamples
  writeIORef normalSamples ss
  return s

squishP xs = M.toList $ M.fromListWith (+) xs
sumP = sum . map snd
normP xs = [(x, p / q) | let q = sumP xs, (x, p) <- xs]

showSamples d = concatMap showRow xs
  where
    xs = (normP . squishP) d
    max_prob = snd $ maximumBy (compare `on` snd) xs :: Double
    showRow (elem, prob) = drawline max_prob prob ++ " | " ++ printf "%.1f (%.1f" elem (prob*100) ++ "%)\n"

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
  show = showSamples . cumulative . collapseIntervals 20 . sample . sequence . replicate 10000 . eval

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

    normal mean std_dev

drawline :: Double -> Double -> String
drawline max n = replicate spaces ' ' ++ replicate normalized ':' where
  spaces = 35 - normalized
  normalized = (round ((n/max) * 30))

intervals :: [Double] -- ^ Each double is a box where all floats less than this number fall, in order (list must be sorted low-to-high).
          -> [Double] -- ^ A list of arbitrary Double samples
          -> [Double] -- ^ A list of samples where all doubles fall into the given discrete categories
intervals boxes dist = do
  s <- dist
  return $ fst $ minimumBy (compare `on` snd) $
    map (\box -> (box, abs (box - s))) {-$ filter (not . isInfinite)-} boxes

cumulative = normP . squishP . map (,1)

-- | Simplify samples into N intervals
collapseIntervals :: Int -> [Double] -> [Double]
collapseIntervals n (sort -> samples) =
  let low  = head samples
      high = last samples
      step = (high - low) / fromIntegral n
   in intervals [low-step, low .. high+step] samples

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
