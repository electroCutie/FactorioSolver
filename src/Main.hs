module Main where

import           Factorio.Recipies

import qualified Data.Map.Strict   as M


factories :: [RecursiveFactory]
factories = M.elems $ recursiveSolver (M.fromList [
    (SpaceSci, 1000/350)
    -- (PurpleSci, 1000/350), (YellowSci, 1000/350),
    -- (RedSci, 1000/350), (GreenSci, 1000/350)
  ])

main :: IO ()
main = do
  putStrLn $ showRecursiveFactories factories
