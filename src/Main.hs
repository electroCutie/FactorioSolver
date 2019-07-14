module Main where

import           Control.Monad.Random.Lazy
import qualified Data.Map.Strict           as M
import           Factorio.Anneal
import           Factorio.Recipies

import           Data.List                 (intercalate)


main :: IO ()
main = do
  let factories = iterativeSolver (M.fromList [(PetGas, 260)])
  putStr $ unlines $ map show $ M.toList factories
  putStrLn ""
  putStr $ unlines $ map show $ M.toList (productionRate factories)
  putStrLn ""
  putStr $ unlines $ map show $ M.toList (netRate factories)
