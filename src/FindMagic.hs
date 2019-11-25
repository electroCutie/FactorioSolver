module Main where

import           Control.Monad (join)
import           Data.List     (find)
import           Data.Maybe
import           Text.Printf

primes :: [Integer]
primes = let
  ps x = test primes where
    lim :: Integer
    lim = floor $ sqrt (fromIntegral x :: Double)
    test ~(n:ns)
      | n > lim = x : ps (x+2)
      | 0 == x `rem` n = ps (x+2)
      | otherwise = test ns
  in 2:3:ps 5

type Segment = ([Integer], [Integer])

-- Segment are given letters from the Cmp with A and going around clockwise to end at the upper left with F, the middle is G and last
segments:: [Segment]
segments = [ -- ([Included [Excluded])
    ([2,3,5,6,7,8,9,0], [1,4]), -- A
    ([1,2,3,4,7,8,9,0], [5,6]), -- B
    ([1,3,4,5,6,7,8,9,0], [2]), -- C
    ([2,3,5,6,8,0], [1,4,7,9]), -- D
    ([2,6,8,0], [1,3,4,5,7,9]), -- E
    ([4,5,6,8,9,0], [1,2,3,7]), -- F
    ([2,3,4,5,6,8,9], [1,7,0])  -- G
  ]

data Cmp = Lt | Gt
data FArgs = FArgs Integer Cmp Integer

applyArgs :: FArgs -> Integer -> Integer
applyArgs (FArgs a _ m) i = let
  x = i+2
  in (a*x*x*x) `mod` m

showTripleFor :: Segment -> FArgs -> String
showTripleFor (as, bs) tr@(FArgs a o m) = s o where
  as' = applyArgs tr <$> as
  bs' = applyArgs tr <$> bs
  p sy ar br = printf "x = i+2; (%2d*x^3) %% %2d : %3d %s %3d" a m (ar as') sy (br bs')
  s Lt = p "<" maximum minimum
  s Gt = p ">" minimum maximum

testArgs :: ([Integer], [Integer]) -> FArgs ->  Bool
testArgs (as, bs) tr@(FArgs _ x _) = t x where
  as' = applyArgs tr <$> as
  bs' = applyArgs tr <$> bs
  t Lt = maximum as' < minimum bs'
  t Gt = minimum as' > maximum bs'

seekMagic :: [FArgs]
seekMagic = let
  aLim :: Integer -> [Integer]
  aLim b = takeWhile (< b) [1..]
  potentials :: Integer -> [FArgs]
  potentials b = (\a o -> FArgs a o b) <$> aLim b <*> [Lt, Gt]
  filtered :: Integer -> Segment -> Maybe FArgs
  filtered b ls = find (testArgs ls) $ potentials b
  possibilitySpace :: [Maybe [FArgs]]
  possibilitySpace = (\p -> sequence (filtered p <$> segments)) <$> primes
  in fromJust $ join $ find isJust possibilitySpace


main :: IO ()
main = putStr $
  unlines $ zipWith (++) ((:": ")<$>['A'..]) $
  zipWith showTripleFor segments seekMagic
