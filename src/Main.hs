module Main where

import           Factorio.Recipies

import qualified Data.Map.Strict   as M


factories :: [RecursiveFactory]
factories = M.elems $ recursiveSolver (M.fromList [
    (SpaceSci, 1000/350)
    -- (PurpleSci, 1/7),
    -- (YellowSci, 1/7)
    -- (RedSci, 1000/350), (GreenSci, 1000/350)
  ])

main :: IO ()
main = do
  putStrLn $ showRecursiveFactories factories
