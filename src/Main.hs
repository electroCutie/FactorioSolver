module Main where

import           Control.Monad.Random.Lazy
import qualified Data.Map.Strict           as M
import           Factorio.Anneal
import           Factorio.Recipies

import           Data.List                 (intercalate)


main :: IO ()
main = do
  putStr $ unlines $ map show $ M.toList $
    topDownSolver (M.singleton EnrichedFuel 5)
