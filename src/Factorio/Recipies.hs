{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Factorio.Recipies where

import           Control.Arrow         (second, (&&&), (***))
import           Data.List             (delete, sortBy)
import           Data.Maybe
import           Data.Ord              (comparing)

import           Control.Monad.Random

import           Data.Map.Merge.Strict
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

import           Factorio.Anneal

import           Debug.Trace

newtype SpeedFactor = SpeedFactor { speed :: Double } deriving (Show, Eq, Ord)
newtype Productivity = Productivity { prod :: Double } deriving (Show, Eq, Ord)

speedMultiplier :: SpeedFactor -> Double
speedMultiplier f = 1.0 / speed f

newtype Time = Time { t :: Double } deriving (Show, Eq, Ord)
newtype Drain = Drain { drain :: Double} deriving (Eq, Show, Ord)

data FType = Assembler | Electronics
  | Refinery | Chemical
  | Compressor | Pump | Electro | Distil
  | Furnace | ChemFurnace | MetalMixing
  | Mining
  | Greenhouse
  deriving (Show, Eq, Ord)

data Machine = Machine {
    mFType :: FType,
    mGen   :: Int,
    mDrain :: Drain,
    mProd  :: Productivity,
    mSpeed :: SpeedFactor
  } deriving (Eq, Ord)
instance Show Machine where
  show (Machine t i _ _ _) = show t ++ show i


data Product = Product Double Double Item deriving (Eq, Show, Ord)
data Ingredient = Ingredient Double Item deriving (Eq, Show, Ord)
class RecipiePart r where
  amt :: r -> Double
  itm :: r -> Item
instance RecipiePart Product where
  amt (Product a _ _) = a
  itm (Product _ _ i) = i
instance RecipiePart Ingredient where
  amt (Ingredient a _) = a
  itm (Ingredient _ i) = i

productPrio :: Product -> Double
productPrio (Product _ x _) = x

data Process = Process {
    pTime        :: Time,
    pFacType     :: FType,
    pIngredients :: [Ingredient],
    pProduct     :: [Product]
  } deriving (Eq, Ord)
instance Show Process where
  show p = [show.pIngredients, const " → ", show.pProduct] >>= ($ p)
data Factory = Factory {
    fMachine :: Machine,
    fProc    :: Process
  } deriving (Eq, Show, Ord)

data Item =
  Water | Air | Hydrogen | Nitrogen | Oxygen | PureWater | Amonia | Peroxide | NOx | NO2 | N2O4 | Hydrazine |
  CrudeOil | HeavyOil | LightOil | PetGas |
  Coal | LiquidFuel | EnrichedFuel | SolidFuel | RocketFuel
  deriving (Eq, Show, Ord)

type ItemCounts = M.Map Item Double

type FactoryN a = M.Map Factory a
type FactoryFloat = FactoryN Double
type FactoryCounts = FactoryN Int

class Production a where
  products :: a -> [Product]
  ingredients :: a -> [Ingredient]
  makesItems :: a -> [Item]
  makesItems = map itm . products
  takesItems :: a -> [Item]
  takesItems = map itm . ingredients
  makesItem :: Item -> a -> Bool
  makesItem i a = elem i $ makesItems a
  takesItem :: Item -> a -> Bool
  takesItem i a = elem i $ takesItems a

  anyMake :: Item -> [a] -> Bool
  anyMake i = any (makesItem i)
  anyTake :: Item -> [a] -> Bool
  anyTake i = any (takesItem i)

  makesAny :: [Item] -> a -> Bool
  makesAny is a = any (flip elem $ makesItems a) is

instance Production Process where
  products    (Process _ _ _ ps) =  ps
  ingredients (Process _ _ is _) = is

instance Production Factory where
  products =  products . fProc
  ingredients = ingredients . fProc

productionRate :: Factory -> ItemCounts
productionRate (Factory m pr) = let
  -- FType Int Drain Productivity SpeedFactor
  (Machine _ _ _ (Productivity p) (SpeedFactor s)) = m
  (Process (Time t) _ _ ps) = pr
  in M.fromList $ map (\x -> (itm x, (1+p) * amt x * s / t)) ps

needRate :: Factory -> ItemCounts
needRate (Factory m pr) = let
  -- FType Int Drain Productivity SpeedFactor
  (Machine _ _ _ _ (SpeedFactor s)) = m
  (Process (Time t) _ is _) = pr
  in M.fromList $ map (\x -> (itm x, amt x * s / t)) is

isRaw :: Process -> Bool
isRaw (Process _ _ is _) = not $ null is

findProcesses :: Item -> [Process] -> ([Process], [Process])
findProcesses i ps = let
  found = filter (makesItem i) ps
  cleaned = foldl (flip delete) ps found
  in (cleaned, found)

findProcessesR :: [Process] -> Item -> [Process]
findProcessesR pss root = go pss [root] where
  go _ [] = []
  go ps (i : is) = let
    (ps', procs) = findProcesses i ps
    next =  itm <$> (procs >>= pIngredients)
    in procs ++ go ps' (is ++ next)

makeFactories :: [Process] -> [Factory]
makeFactories ps = go <$> ps where
  go p = let
    t = pFacType p
    m = filter ((t == ) . mFType) machines
    in Factory (head m) p

calculateNeededFactoriesFrac :: Factory -> Item -> Double -> Double
calculateNeededFactoriesFrac f i r = let
  rawRate = fromJust $ M.lookup i $ productionRate f
  in r / rawRate

calculateNeededFactories :: Factory -> Item -> Double -> Int
calculateNeededFactories f i r = ceiling $ calculateNeededFactoriesFrac f i r

factoriesByItem :: M.Map Item [Factory]
factoriesByItem = let
  fs = makeFactories processes
  fs' = (\f -> (makesItems f, f)) <$> fs
  fs'' = fs' >>= (\(ms, f) -> (,[f]) <$> ms)
  in M.fromListWith (++) fs''

factoriesByItemPrio :: M.Map Item [Factory]
factoriesByItemPrio = let
  getProduct :: Item -> Factory -> Product
  getProduct i f = head $ filter (\x -> i == itm x) $ products f
  sortOnPrio :: Item -> [Factory] -> [Factory]
  sortOnPrio i = sortBy (comparing (productPrio . getProduct i))
  in M.mapWithKey sortOnPrio factoriesByItem


addToCount :: [(Item, Double)] -> ItemCounts -> ItemCounts
addToCount xs m = foldl (flip (uncurry $ M.insertWith (+))) m xs

{- Annealing -}

addMaps :: (Num n, Ord k) => M.Map k n -> M.Map k n -> M.Map k n
addMaps = merge preserveMissing (mapMissing (\_ x -> x)) (zipWithMatched $ const (+))

diffMaps :: (Num n, Ord k) => M.Map k n -> M.Map k n -> M.Map k n
diffMaps = merge preserveMissing (mapMissing (\_ x -> -x)) (zipWithMatched $ const (-))

factoryToRates :: (Factory -> ItemCounts) -> Factory -> Double -> ItemCounts
factoryToRates toCt f ct = M.map (* ct) $ toCt f

factoriesToRates :: (Factory -> ItemCounts) -> FactoryFloat -> ItemCounts
factoriesToRates toCt fs = let
  counts = uncurry (factoryToRates toCt) <$> M.toList fs
  in foldl addMaps M.empty counts


factoriesToNeeds :: FactoryFloat -> ItemCounts
factoriesToNeeds = factoriesToRates needRate
factoriesToMakes :: FactoryFloat -> ItemCounts
factoriesToMakes = factoriesToRates productionRate

factoriesToEnergy :: FactoryCounts -> Double
factoriesToEnergy = M.foldlWithKey' (\a k b -> a + (drain.mDrain.fMachine) k * fromIntegral b ) 0

topDownSolver :: ItemCounts -> FactoryCounts
topDownSolver initialNeeds = let
  iToF = head . (factoriesByItemPrio M.!)
  needF ct fs = M.map (* ct) $ addMaps (factoriesToNeeds fs) initialNeeds
  recursiveSolve :: Item -> Double -> FactoryFloat
  recursiveSolve i rate = let
    fac = iToF i
    facCt = calculateNeededFactoriesFrac fac i rate
    myFac = M.singleton fac facCt
    facNeeds = M.toList $ factoriesToNeeds myFac
    needMaps = uncurry recursiveSolve <$> facNeeds
    in foldl addMaps myFac needMaps
  minimizeSolution :: FactoryCounts -> FactoryCounts
  minimizeSolution initalFacs = M.map ceiling $ go allKeys ffloats where
    allKeys = M.keys initalFacs
    ffloats = M.map fromIntegral initalFacs
    primaryItem = (itm &&& amt).head.pProduct.fProc
    go [] facs       = facs
    go (f : fs) facs = let
      needs = addMaps initialNeeds $ factoriesToNeeds facs
      makes = factoriesToMakes facs
      itemTotals = M.filter (>0) $ diffMaps makes needs
      lessOne = M.insertWith (\a b -> max 0 (a-b)) f 1 facs
      (item, ct) = primaryItem f
      itemExcess = fromMaybe (-1) (itemTotals M.!? item)
      hasExcess  = (facs M.! f > 0) && (itemExcess - ct >= 0)
      in if hasExcess then
           go allKeys lessOne
         else
           go fs facs
  in M.filter (>0)
    $ minimizeSolution
    $ M.map ceiling
    $ foldl addMaps M.empty (uncurry recursiveSolve <$> M.toList initialNeeds )

{- Game Data -}

machines :: [Machine]
machines = ( (\(t, g, d, s) -> Machine t g (Drain d) (Productivity 0) (SpeedFactor s)) <$> [
    -- (Assembler, 1, 100 + 3.3, 0.5),
    -- (Assembler, 2, 135 + 4.5, 0.75),
    (Assembler, 3, 210 + 7, 1.25),
    (Electronics, 2, 213 + 7.1, 2.25),
    (Refinery, 1, 420 + 14, 1),
    (Compressor, 1, 1.6 + 50, 1),
    (Pump, 1, 10, 1),
    (Chemical, 2, 259 + 8.6, 1.75),
    (Electro, 2, 750 + 25, 1.5),
    (Distil, 2, 259 + 8.6, 1.5),
    (Furnace, 2, 90, 2),
    (ChemFurnace, 2, 90, 2),
    (MetalMixing, 2, 90, 2),
    (Greenhouse, 1, 100 + 3.3, 0.75)
  ])
  ++ [
    Machine Mining 2 (Drain 160) (Productivity 0.2) (SpeedFactor 1)
  ]

processes :: [Process]
processes = (\(ps, t, f, is) -> Process (Time t) f (uncurry Ingredient <$> is) ((\(a,b,c) -> Product a b c) <$> ps) ) <$>
  [
-- ([(, )], 1, Chemical, [(, ), (, )]),
  ([( 12, 10, HeavyOil)],                  2.5, Chemical, [(2, Coal), (15, Water)]),
  ([( 20, 10,  Amonia)],                    1,   Chemical, [(10, Nitrogen), (24, Hydrogen)]),
  ([(  8, 10,  N2O4)],                      1,   Chemical, [(20, NOx)]),
  ([(  8, 10,  Hydrazine), (4, 1000, PureWater)], 1,   Chemical, [(20, Amonia), (4, Peroxide)]),
  ([(  8, 10,  Peroxide)],                  1,   Chemical, [(16, Hydrogen), (20, Oxygen)]),
  ([( 20, 10,  NOx), (12, 1000, PureWater)],      1,   Chemical, [(20, Amonia), (25, Oxygen)]),
  ([( 30, 10,  LightOil)],                  2,   Chemical, [(30, Water), (40, HeavyOil)]),
  ([( 20, 10,  PetGas)],                    2,   Chemical, [(30, Water), (30, LightOil)]),
  ([( 20, 10,  NO2)],                       1,   Chemical, [(20, NOx), (10, Oxygen)]),
  ([(100, 10,  PureWater)],                 2,   Distil,   [(100, Water)]),
  ([( 20, 10,  Hydrogen), (12.5, 10, Oxygen)],  1,   Electro,  [(10, PureWater)]),
  ([( 20, 10,  Nitrogen), (5, 20, Oxygen)],     1,   Chemical, [(25, Air)]),
  ([(250, 10,  Hydrogen)],                  2.5, Chemical, [(5, Water), (5, PetGas)]),

  ([(  1, 10,  RocketFuel)],               30,  Chemical,  [(160, Hydrazine), (80, N2O4)]),
  ([( 10, 10,  LiquidFuel)],                1,  Chemical,  [(10, LightOil)]),
  ([(  1, 10,  EnrichedFuel)],             12,  Chemical,  [(1, LiquidFuel)]),
  ([(  1, 10,  SolidFuel)],                 2,  Chemical,  [(10, LightOil)]),
  ([(  1, 20,  EnrichedFuel)],             12,  Chemical,  [(1, SolidFuel), (200, Hydrazine)]),
  ([(  1, 10,  SolidFuel)],                 3,  Chemical,  [(1, Coal), (175, Hydrogen)]),

  ([(1, 10, Coal)],     1, Mining, []),
  ([(1200, 10, Water)], 1, Pump, []),
  ([(100, 10, Air)],    1, Compressor, [])
  ]







-- data Errors err a = Proc a | Missing [err]
-- getErrs :: Errors err a -> [err]
-- getErrs (Missing errs) = errs
-- getErrs _              = []
-- instance Functor (Errors err) where
--   fmap f = go where
--     go (Proc a)     = Proc $ f a
--     go (Missing es) = Missing es
-- instance Applicative (Errors err) where
--   pure = Proc
--   ff <*> fa = go ff fa where
--     go z (Missing es)    = Missing (getErrs z ++ es)
--     go (Missing es) z    = Missing (es ++ getErrs z)
--     go (Proc f) (Proc a) = Proc (f a)
-- instance Monad (Errors err) where
--   return = pure
--   (Missing es) >>= _ = Missing es
--   (Proc a) >>= f     = f a
