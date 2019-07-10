{-# LANGUAGE ScopedTypeVariables #-}

module Factorio.Anneal where

import           Control.Monad.Loops
import           Control.Monad.Random
import           System.Random

data AnnealingParams = AnnealingParams {
    iterationLimit, generationSize :: Int,
    initialTemp                    :: Double,
    cooling                        :: Int -> Double -> Double
  }

anneal :: forall m a. MonadRandom m =>
     AnnealingParams
  -> (Int -> a -> Double)     -- Value Function
  -> (Int -> a -> m a) -- mutate a to a nearby element
  -> a                 -- Initial Position
  -> m a
anneal (AnnealingParams itrLim genSize initialTemp cool) val mutate initial = let
  nextGen g a = replicateM genSize $ mutate g a
  chooseNext :: Int -> Double -> a -> [a] -> m a
  chooseNext g temp a as = let
    s = val g a
    as' = filter (\x -> s + temp >= val g x) as
    in case length as' of
      0   -> return a
      asz -> (as' !!) <$> getRandomR (0, asz-1)
  go :: Int -> Double -> a -> m a
  go 0 _ a = return a
  go g t a = nextGen g a >>= chooseNext g t a >>= go (g-1) (cool g t)
  in go itrLim initialTemp initial
