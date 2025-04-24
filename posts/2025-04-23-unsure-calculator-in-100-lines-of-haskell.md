---

title: Implementing Unsure Calculator in 100 lines of Haskell

description: A lightning post on logic programming in Haskell to construct a
             workout weekly schedule given the set of exercises, days and constraints.

tags: haskell, probability

---

The recently trendy [Unsure Calculator](https://filiph.github.io/unsure/) makes
reasoning about numbers with some uncertainty just as easy as calculating with
specific numbers.

Their key idea is to add a new "range" operator (written `~`) to the vocabulary
of a standard calculator. The range denotes that a real value is uncertain, but
falls somewhere in between the two ends (e.g. `100~500` means a number between
100 and 500). More precisely, the range denotes a normal distribution where each
end is 2 standard deviations from the mean. It means we are 95% sure the real
value falls in between 100 and 500.

> Reading the notation is easy: when you see `10~15`, you say: "ten to fifteen".

In the unsure calculator, arithmetic operations and the remaining usual
suspects, such as `sin` or `log`, transparently operate on ranges and literal
numbers alike. For instance:

```
1400~1700 * 0.55~0.65 - 600~700 - 100~200 - 30 - 20 = -60~220
```

This expression can also be plotted out in the original website:

```
     above | ▒▒▒▒
       236 | ▒▒▒
       221 | ▒▒▒▒
       205 | ▒▒▒▒▒▒
       190 | ▒▒▒▒▒▒▒▒▒
       175 | ▒▒▒▒▒▒▒▒▒▒▒▒
       159 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       144 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       129 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       114 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        98 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        83 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ (79)
        68 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        52 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        37 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        22 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
         6 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
        -9 | ▒▒▒▒▒▒▒▒▒▒▒▒▒▒
       -24 | ▒▒▒▒▒▒▒▒▒▒
       -39 | ▒▒▒▒▒▒▒
       -55 | ▒▒▒▒▒
       -70 | ▒▒▒
     below | ▒▒▒▒
```

The motivation behind the original article is neat, so I'll just recommend you
read it there to learn how and why you'd use such a calculator. They also
explain the real life story for the numbers in the example above.


```
{- cabal:
build-depends: base, random, containers
-}
{-# LANGUAGE GADTs, ViewPatterns, GHC2021 #-}
import Text.Printf
import Control.Monad (liftM, ap)
import Data.Function (on)
import Data.List (sort, minimumBy)
import System.Random
import qualified Data.Map as M

--- Probability Monad ----------------------------------------------------------

-- https://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf
data Dist a where
  Return :: a -> Dist a
  Bind   :: Dist b -> (b -> Dist a) -> Dist a
  Normal :: Double -> Double -> Dist Double

instance Monad       Dist where (>>=) = Bind
instance Applicative Dist where pure  = Return; (<*>) = ap
instance Functor     Dist where fmap  = liftM

sample :: StdGen -> Dist a -> a
sample g d = case d of
  Return x -> x
  Normal mean std_dev -> n1 * std_dev + mean
    where ((u1, u2), _) = uniformR ((0,0), (1,1)) g
          (n1,  _)      = boxMuller u1 u2
  Bind d f -> sample g1 (f (sample g2 d))
    where (g1, g2) = splitGen g

boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
                                               t = 2 * pi * u2

--- Expressions ----------------------------------------------------------------

data Expr
  = Num Double
  | Add Expr Expr   | Mul Expr Expr
  | Abs Expr        | Signum Expr
  | Negate Expr     | Div Expr Expr
  | Exp Expr        | Log Expr
  | Sin Expr        | Cos Expr
  | Range Expr Expr

(~) = Range

instance Num Expr where
  (+) = Add; (*) = Mul
  negate = Negate; abs = Abs
  signum = Signum; fromInteger = Num . fromInteger

instance Fractional Expr where
  (/) = Div; fromRational = Num . fromRational

instance Floating Expr where
  pi = Num pi; exp = Exp; log = Log; sin = Sin; cos = Cos

eval :: Expr -> Dist Double
eval e = case e of
  Num d       -> return d
  Add e1 e2   -> (+) <$> eval e1 <*> eval e2
  Mul e1 e2   -> (*) <$> eval e1 <*> eval e2
  Negate e    -> negate <$> eval e
  Abs e       -> abs <$> eval e
  Signum e    -> signum <$> eval e
  Div e1 e2   -> (/) <$> eval e1 <*> eval e2
  Exp e       -> exp <$> eval e
  Log e       -> log <$> eval e
  Sin e       -> sin <$> eval e
  Cos e       -> cos <$> eval e
  Range e1 e2 -> do
    a <- eval e1
    b <- eval e2
    let mean = (a + b) / 2
        std_dev = (b - a) / 4
    Normal mean std_dev

--- Drawing --------------------------------------------------------------------

instance Show Expr where
  show e = concatMap showRow intervals where
    samples = take 25000 . sample (mkStdGen 0) . sequence . repeat . eval
    intervals = collapseIntervals 40 (samples e)
    line p = replicate spaces ' ' ++ replicate normalized ':'
      where spaces = 35 - normalized
            normalized = round ((p/max_prob) * 30)
            max_prob = maximum $ map snd intervals
    showRow (elem, prob) = line prob ++ " | " ++ printf "%.1f (%.1f" elem (prob*100) ++ "%)\n"

collapseIntervals :: Int -> [Double] -> [(Double, Double)]
collapseIntervals n (sort -> samples) =
  let (low, high) = (head samples, last samples)
      step  = (high - low) / fromIntegral n
      boxes = [low, low+step .. high] 
      total = fromIntegral (length samples)
   in M.toList $ M.map (/total) $ M.fromListWith (+)
        [ (minimumBy (compare `on` \box -> abs (box - s)) boxes, 1)
        | s <- samples ]
```
