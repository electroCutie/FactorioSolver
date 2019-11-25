{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Factorio.RecipiesBob where

import           Control.Arrow         (first, second, (&&&), (***))
import           Data.List             (delete, intercalate, sortBy)
import           Data.Maybe
import           Data.Ord              (comparing)

import           Control.Monad.Random

import           Data.Map.Merge.Strict
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

import           Text.Printf

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


data Product = Product Double Double Item deriving (Eq, Ord)
instance Show Product where
  show (Product a _ i) = show a ++ "x " ++ show i
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
    pProduct     :: [Product],
    pIngredients :: [Ingredient]
  } deriving (Eq, Ord)
instance Show Process where
  show p = [show.pIngredients, const " → ", show.pProduct] >>= ($ p)

data Factory = Factory {
    fMachine :: Machine,
    fProc    :: Process
  } deriving (Eq, Show, Ord)

showFac :: (Factory, Int) -> String
showFac (f, i) = printf "% 4dx % 12s  {%s} ← {%s}" i (show $ fMachine f) outs ins where
  d    = fromIntegral i
  ins  = showR $ needRate f *: d
  outs = showR  $ productionRate f *: d
  showR :: ItemCounts -> String
  showR m = intercalate ", " ((\(i, c) -> printf "(%s %.2f/s)" (show i) c) <$> M.toList m)


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
  products    (Process _ _ ps _) = ps
  ingredients (Process _ _ _ is) = is

instance Production Factory where
  products =  products . fProc
  ingredients = ingredients . fProc

class ToDouble n where
  toDouble :: n -> Double
instance ToDouble Double where
  toDouble = id
instance ToDouble Int where
  toDouble = fromIntegral

class ProductionRate p  where
    needRate :: p -> ItemCounts
    productionRate :: p -> ItemCounts
    netRate :: p -> ItemCounts
    netRate p = productionRate p - needRate p

instance ProductionRate Factory where
  productionRate (Factory m pr) = let
    -- FType Int Drain Productivity SpeedFactor
    (Machine _ _ _ (Productivity p) (SpeedFactor s)) = m
    (Process (Time t) _ ps _) = pr
    in M.fromList $ map (\x -> (itm x, (1+p) * amt x * s / t)) ps

  needRate (Factory m pr) = let
    -- FType Int Drain Productivity SpeedFactor
    (Machine _ _ _ _ (SpeedFactor s)) = m
    (Process (Time t) _ _ is) = pr
    in M.fromList $ map (\x -> (itm x, amt x * s / t)) is

instance (ToDouble n) => ProductionRate (M.Map Factory n) where
  productionRate fs = sum $ uncurry (*:) <$> ((productionRate *** toDouble) <$> M.toList fs)
  needRate fs = sum $ uncurry (*:) <$> ((needRate *** toDouble) <$> M.toList fs)

isRaw :: Process -> Bool
isRaw (Process _ _ _ is) = not $ null is

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

preferedFactory :: Item -> Factory
preferedFactory = head . getE where
  getE k = case factoriesByItemPrio M.!? k of
    (Just x) -> x
    _        -> error ("No Factory for " ++ show k)

addToCount :: [(Item, Double)] -> ItemCounts -> ItemCounts
addToCount xs m = foldl (flip (uncurry $ M.insertWith (+))) m xs

instance (Ord a, Num n) => Num (M.Map a n) where
  (+) = merge preserveMissing (mapMissing (\_ x -> x)) (zipWithMatched $ const (+))
  (-) = merge preserveMissing (mapMissing (\_ x -> -x)) (zipWithMatched $ const (-))
  (*) = merge dropMissing dropMissing (zipWithMatched $ const (*))
  negate = M.map negate
  abs = M.map abs
  signum = M.map signum
  fromInteger 0 = M.empty
  fromInteger _ = error "No logical map for a nonzero number"

(*:) :: (Ord a, Num n) => M.Map a n -> n -> M.Map a n
(*:) m n = M.map (*n) m
infixl 7 *:

factoryToRates :: (Factory -> ItemCounts) -> Factory -> Double -> ItemCounts
factoryToRates toCt f ct = M.map (* ct) $ toCt f

factoriesToRates :: (Factory -> ItemCounts) -> FactoryFloat -> ItemCounts
factoriesToRates toCt fs = let
  counts = uncurry (factoryToRates toCt) <$> M.toList fs
  in sum counts


factoriesToNeeds :: FactoryFloat -> ItemCounts
factoriesToNeeds = factoriesToRates needRate
factoriesToMakes :: FactoryFloat -> ItemCounts
factoriesToMakes = factoriesToRates productionRate

factoriesToEnergy :: FactoryCounts -> Double
factoriesToEnergy = M.foldlWithKey' (\a k b -> a + (drain.mDrain.fMachine) k * fromIntegral b ) 0

iterativeSolver :: ItemCounts -> FactoryCounts
iterativeSolver initialNeeds = let
  iToF = preferedFactory
  needF :: FactoryCounts -> ItemCounts
  needF fs = abs $ M.filter (< -0.001) $ netRate fs - initialNeeds
  itr :: FactoryCounts -> FactoryCounts
  itr fs
    | haveAll   = fs
    | otherwise = itr (fs + nFacs)
    where
      needs     = needF fs
      firstNeed = head $ M.keys needs
      firstRate = needs M.! firstNeed
      nFacs     = let
        f = iToF firstNeed
        n = calculateNeededFactories f firstNeed firstRate
        in M.singleton f (max 1 n)
      haveAll   = M.null needs
  in itr M.empty

{- Game Data -}

data Material = Iron | Copper | Tin | Lead | Nickel | Brass | Bronze | Steel | Cobalt | CobaltS | Titanium | Tungsten |
  Silicon | Gold | Silver | Zinc | Aluminium | Ceramic | Invar | Lithium
  deriving (Eq, Show, Ord)

data TechTier     = Grey | Yellow | Red | Blue deriving (Eq, Show, Ord)
data BoardVersion = Bare | Populated deriving (Eq, Show, Ord)
data InserterType = Simple | Filter | Stack | StackFilter deriving (Eq, Show, Ord)
data Warhead      = AP | HE | Frag deriving (Eq, Show, Ord)
data BattTech     = LeadAcid | LiOn | SilverZinc deriving (Eq, Show, Ord)
data Mk           = Mk1 | Mk2 | Mk3  deriving (Eq, Show, Ord)
data BFrame       = Fighting | Flying deriving (Eq, Show, Ord)
data BotType      = Battle | Logistics | Construction deriving (Eq, Show, Ord)

data Item =
  Inserter TechTier InserterType | Belt TechTier | Splitter TechTier | Underground TechTier |
  YellowSci | RedSci | BlueSci | MilSci |
  CBoard BoardVersion TechTier |
  Battery BattTech |
  Lithia | LiCl | LiCoO2 | LiClO4 | Si3N4 |
  Hydrogen | Nitrogen | Oxygen | PureWater | Amonia | Peroxide |
  Salt | NaOH | Chlorine | Plastic | Carbon | SiliconPowder |
  NOx | NO2 | N2O4 | Hydrazine | SO2 | H2SO4 | HNO3 | CaCl2 | HCl | Ferric |
  CrudeOil | HeavyOil | LightOil | PetGas |
  Coal | LiquidFuel | EnrichedFuel | SolidFuel | RocketFuel |
  LeadOxide | SolderPlate | Solder |
  CobaltOxide | Alumina |
  TungstenAcid | TungstenOxide | TungstenPowder |
  Fertilizer | Seedling | Wood | Resin | Rubber |
  Engine | Lube | EMotor |
  BotFrame BFrame Mk | BotBrain BotType Mk | BotTool BotType Mk |
  BotDoor Mk     | BotAntenna Mk    | BotCharge Mk |
  Gear Material  | Bearing Material | Ball Material |
  Ore Material   | Plate Material   | Pipe Material  |
  WoodBoard | PhenolicBoard | FiberglassBoard |
  Wafer | Glass |
  CopperWire | TinnedWire | InsulatedWire | Resistor | Transistor | IC |
  Brick | Concrete | ReinforcedConcrete |
  Magazine | Magazine2 | Grenade | Wall | LeadShot | Explosive | CannonShell Warhead |
  Stone | Sulphur | Water | Air

  deriving (Eq, Show, Ord)

machines :: [Machine]
machines = ( (\(t, g, d, s) -> Machine t g (Drain d) (Productivity 0) (SpeedFactor s)) <$> [
    -- (Assembler, 1, 100 + 3.3, 0.5),
    -- (Assembler, 2, 135 + 4.5, 0.75),
    (Assembler, 5, 210 + 7, 2.75),
    (Electronics, 2, 213 + 7.1, 2.25),
    (Refinery, 1, 420 + 14, 1),
    (Compressor, 3, 1.6 + 90, 3.5 ),
    (Pump, 1, 10, 1),
    (Chemical, 3, 259 + 8.6, 2.75),
    (Electro, 3, 750 + 25, 2.5),
    (Distil, 2, 259 + 8.6, 1.5),
    (Furnace, 5, 180 + 6, 3),
    (ChemFurnace, 5, 180, 3),
    (MetalMixing, 5, 90, 3),
    (Greenhouse, 1, 100 + 3.3, 0.75)
  ])
  ++ [
    Machine Mining 4 (Drain 160) (Productivity 0.2) (SpeedFactor 4)
  ]

processes :: [Process]
processes = (\(ps, t, f, is) -> Process (Time t) f ((\(a,b,c) -> Product a b c) <$> ps) (uncurry Ingredient <$> is) ) <$>
  ([
-- ([(, )], 1, Chemical, [(, ), (, )]),
  ([( 12, 10,  HeavyOil)],                2.5,   Chemical,     [(2, Coal), (15, Water)]),
  ([( 30, 10,  LightOil)],                  2,   Chemical,     [(30, Water), (40, HeavyOil)]),
  ([( 20, 10,  PetGas)],                    2,   Chemical,     [(30, Water), (30, LightOil)]),
  ([( 20, 10,  Amonia)],                    1,   Chemical,     [(10, Nitrogen), (24, Hydrogen)]),
  ([(  8, 10,  N2O4)],                      1,   Chemical,     [(20, NO2)]),
  ([(  8, 10,  Hydrazine), (4, 1000, PureWater)], 1,   Chemical, [(20, Amonia), (4, Peroxide)]),
  ([(  8, 10,  Peroxide)],                  1,   Chemical,     [(16, Hydrogen), (20, Oxygen)]),
  ([( 20, 10,  NOx), (12, 1000, PureWater)],1,   Chemical,     [(20, Amonia), (25, Oxygen)]),
  ([( 20, 10,  NO2)],                       1,   Chemical,     [(20, NOx), (10, Oxygen)]),
  ([(100, 10,  PureWater)],                 2,   Distil,       [(100, Water)]),
  ([( 20, 20,  Hydrogen), (12.5, 10, Oxygen)],  1,   Electro,  [(10, PureWater)]),
  ([( 20, 10,  Nitrogen), (5, 20, Oxygen)], 1,   Chemical,     [(25, Air)]),
  ([(250, 10,  Hydrogen)],                2.5,   Chemical,     [(5, Water), (5, PetGas)]),

  ([(  1, 10,  RocketFuel)],               30,   Chemical,     [(160, Hydrazine), (80, N2O4)]),
  ([( 10, 10,  LiquidFuel)],                1,   Chemical,     [(10, LightOil)]),
  ([(  1, 10,  EnrichedFuel)],             12,   Chemical,     [(20, LiquidFuel)]),
  ([(  1, 10,  SolidFuel)],                 2,   Chemical,     [(10, LightOil)]),
  ([(  1, 20,  EnrichedFuel)],             12,   Chemical,     [(1, SolidFuel), (200, Hydrazine)]),
  ([(  1, 10,  SolidFuel)],                 3,   Chemical,     [(1, Coal), (175, Hydrogen)]),

  ([(  1, 10,  Glass)],                   3.2,   Furnace,      [(1, Ore Silicon)]),
  ([(  1, 10,  Plate Iron)],              3.2,   Furnace,      [(1, Ore Iron)]),
  ([(  1, 10,  Plate Copper)],            3.2,   Furnace,      [(1, Ore Copper)]),
  ([(  1, 10,  Plate Tin)],               3.2,   Furnace,      [(1, Ore Tin)]),
  ([(  1, 10,  Plate Steel)],             3.2,   ChemFurnace,  [(1, Plate Iron), (10, Oxygen)]),
  ([(  1, 10,  Plate Gold)],              3.2,   ChemFurnace,  [(1, Ore Gold), (3, Chlorine)]),
  ([(  2, 10,  Plate Titanium)],          6.4,   Electro,      [(1, Carbon), (2, CaCl2), (2, Ore Titanium)]),
  ([(  1, 10,  Plate Zinc)],              3.2,   Electro,      [(1, Ore Zinc), (10, H2SO4)]),
  ([(  1, 10,  Plate Lead)],              3.2,   Furnace,      [(1, Ore Lead)]),
  ([(  1, 10,  Alumina)],                 2,     ChemFurnace,  [(1, Ore Aluminium), (1, NaOH)]),
  ([(  2, 10,  Plate Aluminium)],         6.4,   Electro,      [(2, Alumina), (1, Carbon)]),


  ([(  1, 10,  Plate Nickel), (10, 30, SO2)],  3.2,    Electro,      [(1, Ore Nickel), (8, Oxygen)]),
  ([( 50, 10,  H2SO4)],                    1,    Chemical,     [(50, Water), (50, SO2)]),
  ([( 20, 10,  HNO3)],                     1,    Chemical,     [(20, Water), (20, NO2)]),
  ([(  1, 10,  Fertilizer)],               3,    Chemical,     [(10, Amonia), (10, HNO3)]),

  ([(  2, 10,  CobaltOxide)],              7,    ChemFurnace,  [(2, Ore Cobalt), (1, Stone)]),
  ([(  1, 10,  Plate Cobalt)],           3.2,    ChemFurnace,  [(1, CobaltOxide), (10, H2SO4)]),
  ([( 10, 10,  Plate CobaltS)],           32,    MetalMixing,  [(14, Plate Iron), (1, Plate Cobalt)]),
  ([(  1, 10,  Plate Lithium)],          3.2,    Electro,      [(1, LiCl)]),

  ([(  5, 10,  Plate Brass)],             16,    MetalMixing,  [(3, Plate Copper), (2, Plate Zinc)]),
  ([(  5, 10,  Plate Bronze)],            16,    MetalMixing,  [(3, Plate Copper), (2, Plate Tin)]),
  ([(  5, 10,  Plate Invar)],             16,    MetalMixing,  [(3, Plate Iron), (2, Plate Nickel)]),

  ([( 20, 10, TungstenAcid), (1, 20, CaCl2)], 2, Chemical,     [(2, Ore Tungsten), (25, HCl)]),
  ([(  1, 10, TungstenOxide)],             2,    ChemFurnace,  [(10, TungstenAcid)]),
  ([(  1, 10, TungstenPowder)],          3.5,    ChemFurnace,  [(1, TungstenOxide),  (15, Oxygen)]),
  ([(  5, 10, Plate Tungsten)],           16,    MetalMixing,  [(4, TungstenPowder), (1, Plate Nickel)]),


  ([(25, 10, Chlorine), (20, 30, Hydrogen), (1, 10, NaOH)],
                                          2,    Electro,       [(10, PureWater), (1, Salt)]),

  ([(25, 10, HCl)],                       1,     Chemical,     [(12.5, Chlorine), (10, Hydrogen)]),
  ([(1,  10, CaCl2)],                     1,     Chemical,     [(1, Stone), (10, HCl)]),
  ([(50, 10, SO2)],                       1,     Chemical,     [(5, Sulphur), (50, Oxygen)]),
  ([( 1, 10, Salt)],                    0.5,     ChemFurnace,  [(25, Water)]),
  ([( 2, 10, Plastic)],                   1,     Chemical,     [(20, PetGas), (10, Chlorine)]),
  ([( 2, 10, Carbon)],                    2,     Chemical,     [(5, Water), (1, Coal)]),
  ([(50, 10, Ferric)],                  2.5,     Chemical,     [(1, Ore Iron), (30, HCl)]),

  ([( 2, 10,  Plate Silicon)],          6.4,     Electro,      [(2, Ore Silicon), (1, Carbon), (2, CaCl2)]),
  ([( 1, 10,  SiliconPowder)],          6.4,     Assembler,    [(1, Ore Silicon)]),
  ([( 1, 10,  Si3N4)],                  7.5,     ChemFurnace,  [(1, SiliconPowder), (12.5, Nitrogen)]),

  ([(50,   10, Ferric)],                2.5,     Chemical,     [(1, Ore Iron), (30, HCl)]),
  ([( 5,   10, Seedling)],              0.5,     Assembler,    [(1, Wood)]),
  ([( 1,   10, Resin)],                   1,     Assembler,    [(1, Wood)]),
  ([( 1,   10, Rubber)],                3.5,     ChemFurnace,  [(1, Resin)]),
  ([(11,   10, SolderPlate)],             7,     MetalMixing,  [(7, Plate Lead), (4, Plate Tin)]),
  ([(8,    10, Solder)],                  2,     Electronics,  [(1, Resin), (4, SolderPlate)]),
  ([(3,    10, TinnedWire)],            0.5,     Electronics,  [(3, CopperWire), (1, Plate Tin)]),
  ([(2,    10, InsulatedWire)],         0.5,     Electronics,  [(2, TinnedWire), (1, Rubber)]),
  ([(5,    10, Resistor)],                2,     Electronics,  [(1, TinnedWire), (1, Carbon)]),
  ([(8,    10, Wafer)],                   5,     Assembler,    [(1, Plate Silicon)]),
  ([(5,    10, Transistor)],            3.5,     Electronics,  [(1, Plastic), (2, Wafer), (1, TinnedWire)]),
  ([(5,    10, IC)],                      5,     Electronics,  [(1, Plastic), (4, Wafer), (1, TinnedWire), (5, H2SO4)]),
  ([(2,    10, FiberglassBoard)],       0.5,     Electronics,  [(1, Plastic), (1, Glass)]),
  ([(1,    10, CBoard Bare Blue)],       10,     Electronics,  [(1, Plate Copper), (1, Plate Gold), (1, FiberglassBoard), (5, Ferric)]),
  ([(1,    10, CBoard Populated Blue)],  10,     Electronics,  [(2, Solder), (2, Resistor), (4, Transistor), (2, IC), (1, CBoard Bare Blue)]),

  ([(2,   10, CopperWire)],             0.5,     Electronics,  [(1, Plate Copper)]),
  ([(2,   10, WoodBoard)],              0.5,     Electronics,  [(1, Wood)]),
  ([(1,   10, CBoard Bare Yellow)],       1,     Electronics,  [(1, WoodBoard), (3, CopperWire)]),
  ([(1,   10, CBoard Populated Yellow)],  1,     Electronics,  [(1, Solder), (1, CBoard Bare Yellow), (5, Resistor)]),

  ([(2,   10, PhenolicBoard)],          0.5,     Electronics,  [(1, Wood), (1, Resin)]),
  ([(1,   10, CBoard Bare Red)],          5,     Electronics,  [(1, PhenolicBoard), (1, Plate Copper), (1, Plate Tin), (5, Ferric)]),
  ([(1,   10, CBoard Populated Red)],     5,     Electronics,  [(1, Solder), (1, CBoard Bare Red), (5, Resistor), (4, Transistor)]),


  ([(1,   10, Gear Iron)],              0.5,     Assembler,    [(2, Plate Iron)]),
  ([(1,   10, Pipe Iron)],              0.5,     Assembler,    [(1, Plate Iron)]),
  ([(1,   10, Engine)],                  10,     Assembler,    [(1, Plate Steel), (1, Gear Iron), (2, Pipe Iron)]),
  ([(10,  10, Lube)],                     1,     Chemical,     [(10, HeavyOil)]),
  ([(1,   10, EMotor)],                  10,     Assembler,    [(1, Engine), (2, CBoard Populated Yellow), (15, Lube)]),

  ([(2,   10, Belt Grey)],              0.5,     Assembler,    [(1, Plate Iron), (1, Gear Iron)]),
  ([(1,   10, Belt Yellow)],            0.5,     Assembler,    [(2, Gear Iron), (2, Plate Tin), (1, Belt Grey)]),
  ([(1,   10, Belt Red)],               0.5,     Assembler,    [(4, Gear Steel), (2, Plate Bronze), (1, Belt Yellow)]),
  ([(1,   10, Belt Blue)],              0.5,     Assembler,    [(4, Gear CobaltS), (4, Bearing CobaltS), (2, Plate Aluminium), (1, Belt Red )]),

  ([(2,   10, Underground Grey)],         1,     Assembler,    [(2, Wood), (2, Stone), (5, Belt Grey)]),
  ([(2,   10, Underground Yellow)],       1,     Assembler,    [(20, Gear Iron), (14, Plate Tin), (2, Underground Grey)]),
  ([(2,   10, Underground Red)],          2,     Assembler,    [(20, Gear Steel), (14, Plate Bronze), (2, Underground Yellow)]),
  ([(2,   10, Underground Blue)],         2,     Assembler,    [(20, Gear CobaltS), (20, Bearing CobaltS), (14, Plate Aluminium), (2, Underground Red)]),

  ([(1,   10, Splitter Grey)],            1,     Assembler,    [(4, Wood), (4, CopperWire), (2, Gear Iron), (5, Belt Grey)]),
  ([(1,   10, Splitter Yellow)],          1,     Assembler,    [(14, Gear Iron), (8, Plate Tin), (5, CBoard Bare Yellow), (1, Splitter Grey)]),
  ([(1,   10, Splitter Red)],             2,     Assembler,    [(14, Gear Steel), (8, Plate Bronze), (5, CBoard Populated Yellow), (1, Splitter Yellow)]),
  ([(1,   10, Splitter Blue)],            2,     Assembler,    [(14, Gear CobaltS), (12, Bearing CobaltS), (8, Plate Aluminium), (5, CBoard Populated Red), (1, Splitter Red)]),

  ([(1,   10, Inserter Yellow Simple)], 0.5,     Assembler,    [(1, Plate Iron), (1, Gear Iron), (1, CBoard Bare Yellow)]),
  ([(1,   10, Inserter Red Simple)],    0.5,     Assembler,    [(1, Inserter Yellow Simple), (1, Plate Bronze), (1, Gear Steel), (1, CBoard Populated Yellow)]),
  ([(1,   10, Inserter Blue Simple)],   0.5,     Assembler,    [(1, Inserter Red Simple), (1, Plate Aluminium), (1, Gear CobaltS), (1, Bearing CobaltS), (1, CBoard Populated Red)]),

  ([(1,   10, Inserter Yellow Filter)], 0.5,     Assembler,    [(1, Inserter Yellow Simple), (4, CBoard Populated Yellow)]),
  ([(1,   10, Inserter Red Filter)],    0.5,     Assembler,    [(1, Inserter Yellow Filter), (1, Plate Bronze), (1, Gear Steel), (1, CBoard Populated Yellow)]),
  ([(1,   10, Inserter Blue Filter)],   0.5,     Assembler,    [(1, Inserter Red Filter), (1, Plate Aluminium), (1, Gear CobaltS), (1, Bearing CobaltS), (5, CBoard Populated Red)]),

  ([(1,   10, Inserter Red Stack)],     0.5,     Assembler,    [(1, Inserter Red Simple), (3, Plate Bronze), (5, Gear Steel)]),
  ([(1,   10, Inserter Blue Stack)],    0.5,     Assembler,    [(1, Inserter Red Stack),  (4, Plate Aluminium), (6, Gear CobaltS), (5, Bearing CobaltS), (1, CBoard Populated Red)]),

  ([(1,   10, Inserter Red StackFilter)], 0.5,   Assembler,    [(1, Inserter Red Stack), (5, CBoard Populated Yellow)]),
  ([(1,   10, Inserter Blue StackFilter)],0.5,   Assembler,    [(1, Inserter Red StackFilter),  (4, Plate Aluminium), (6, Gear CobaltS), (6, Bearing CobaltS), (6, CBoard Populated Red)]),

  ([(1,   10, YellowSci)],                5,     Assembler,    [(1, Plate Copper), (1, Gear Iron)]),
  ([(2,   10, MilSci)],                  10,     Assembler,    [(1, Magazine2), (1, Grenade), (2, Wall)]),

  ([(1,   10, Battery LeadAcid)],         4,     Chemical,     [(1, Plastic), (2, Plate Lead), (20, H2SO4)]),

  ([(1,   10, LiCl)],                     0.5,   Chemical,     [(25, Lithia)]),
  ([(1,   10, LiCoO2)],                     7,   ChemFurnace,  [(1, LiCl), (1, CobaltOxide)]),

  ([(2,   10, LiCoO2)],                     7,   ChemFurnace,  [(1, Plate Lithium), (1, CobaltOxide)]),
  ([(1,   10, LiClO4), (10, 30, Hydrogen)], 1,   Electro,      [(1, LiCl), (10, PureWater)]),
  ([(1,   10, Battery LiOn)],               4,   Assembler,    [(1, Plastic), (1, Carbon), (1, LiCoO2), (2, LiClO4)]),

  ([(1,   10, BotFrame Flying Mk1)],       20,   Assembler,    [(1, Plate Steel), (2, Battery LeadAcid), (1, EMotor), (3, CBoard Populated Yellow)]),
  ([(1,   10, BotFrame Flying Mk2)],       20,   Assembler,    [(1, Plate Aluminium), (2, Battery LeadAcid), (1, EMotor), (3, CBoard Populated Red)]),
  ([(1,   10, BotBrain Construction Mk1)],  5,   Electronics,  [(5, Solder), (8, Resistor), (3, Transistor), (1, CBoard Bare Red)]),
  ([(1,   10, BotBrain Construction Mk2)],  5,   Electronics,  [(5, Solder), (12, Resistor), (4, Transistor), (1, CBoard Bare Red)]),
  ([(1,   10, BotBrain Logistics Mk1)],     5,   Electronics,  [(5, Solder), (6, Resistor), (4, Transistor), (1, CBoard Bare Red)]),
  ([(1,   10, BotBrain Logistics Mk2)],     5,   Electronics,  [(5, Solder), (10, Resistor), (6, Transistor), (1, CBoard Bare Red)]),
  ([(1,   10, BotTool Construction Mk1)],   1,   Assembler,    [(1, Plate Steel), (2, Gear Steel)]),
  ([(1,   10, BotTool Construction Mk2)],   1,   Assembler,    [(1, Plate Aluminium), (2, Gear Brass), (2, Bearing Steel)]),
  ([(1,   10, BotTool Logistics Mk1)],      1,   Assembler,    [(1, Plate Steel), (2, Gear Steel)]),
  ([(1,   10, BotTool Logistics Mk2)],      1,   Assembler,    [(1, Plate Aluminium), (2, Gear Brass), (2, Bearing Steel)]),
  -- BotDoor Mk     | BotAntenna Mk    | BotCharge Mk |
  ([(1,   10, BotAntenna Mk1)],           0.2,   Assembler,    [(1, Plate Steel), (2, CopperWire), (5, CBoard Populated Red)]),
  ([(1,   10, BotAntenna Mk2)],           0.2,   Assembler,    [(1, Plate Aluminium), (2, TinnedWire), (5, CBoard Populated Red)]),
  ([(1,   10, BotAntenna Mk3  )],         0.2,   Assembler,    [(1, Plate Nickel), (2, InsulatedWire), (5, CBoard Populated Blue)]),
  ([(1,   10, BotDoor Mk1)],                1,   Assembler,    [(15, Plate Steel), (20, Gear Iron)]),
  ([(1,   10, BotDoor Mk2)],                1,   Assembler,    [(15, Plate Invar), (20, Gear Brass), (20, Bearing Steel)]),
  ([(1,   10, BotDoor Mk3)],                1,   Assembler,    [(15, Plate Titanium), (20, Gear Titanium), (20, Bearing Titanium)]),
  ([(1,   10, BotCharge Mk1)],              1,   Assembler,    [(2, Plate Steel), (5, CBoard Populated Red)]),
  ([(1,   10, BotCharge Mk2)],              1,   Assembler,    [(2, Battery LeadAcid), (2, Plate Invar), (5, CBoard Populated Red)]),
  ([(1,   10, BotCharge Mk3)],              1,   Assembler,    [(2, Battery LiOn), (2, Plate Titanium), (5, CBoard Populated Blue)]),

  ([(1,   10, Magazine)],                 1,     Assembler,    [(4, Plate Iron)]),
  ([(1,   10, Magazine2)],                3,     Assembler,    [(5, Plate Copper), (1, Plate Steel), (1, Magazine)]),
  ([(1,   10, Brick)],                  3.2,     Furnace,      [(2, Stone)]),
  ([(10,  10, Concrete)],                10,     Assembler,    [(5, Brick), (1, Ore Iron), (100, Water)]),
  ([(1,   10, Wall)],                   0.5,     Assembler,    [(5, Brick)]),
  ([(1,   10, Grenade)],                  8,     Assembler,    [(10, Coal), (5, Plate Iron)]),
  ([(1,   10, LeadShot)],               0.5,     Assembler,    [(1, Plate Lead)]),
  ([(2,   10, Explosive)],                4,     Assembler,    [(1, Coal), (1, Sulphur), (10, Water)]),
  ([(1,   10, CannonShell Frag)],         8,     Assembler,    [(4, Plate Steel), (2, Plastic), (1, Explosive), (5, LeadShot)]),

  ([(30,   10, Wood)],                   45,     Greenhouse,   [(5, Fertilizer), (10, Seedling), (20, Water)]),
  ([(1,    10, Coal)],                    1,    Mining, []),
  ([(1200, 10, Water)],                   1,    Pump, []),
  ([(100,  10, Air)],                     1,    Compressor, []),
  ([(1,    10, Ore Iron)],                1,    Mining, []),
  ([(1,    10, Ore Tin)],                 1,    Mining, []),
  ([(1,    10, Ore Aluminium)],           1,    Mining, []),
  ([(1,    10, Ore Nickel), (1, 10, Ore Lead)],  1/0.75, Mining, []),
  ([(1,    10, Ore Copper)],              1,    Mining, []),
  ([(1,    10, Ore Gold)],                1,    Mining, []),
  ([(1,    10, Ore Titanium)],            1,    Mining, []),
  ([(1,    10, Ore Zinc)],                1,    Mining, []),
  ([(1,    10, Ore Tungsten)],            1,    Mining, []),
  ([(1,    10, Ore Cobalt)],              1,    Mining, []),
  ([(1,    10, Ore Silicon)],             1,    Mining, []),
  ([(2500, 10, Lithia)],                  0.1,  Pump, []),
  ([(1,    10, Stone)],                   1,    Mining, []),
  ([(1,    10, Sulphur)],                 1,    Mining, [])
  ]
  ++
  ((\m -> ([(1,   10, Gear m)],            0.5,     Assembler,    [(1, Plate m)]) ) <$> [Steel, Brass, CobaltS, Titanium, Tungsten])
  ++
  ((\m -> ([(12,   10, Ball m)],           0.5,     Assembler,    [(1, Plate m)]) ) <$> [Steel, CobaltS, Titanium, Ceramic])
  ++
  ((\m -> ([(12,   10, Bearing m)],        0.5,     Assembler,    [(16, Ball m), (1, Plate m)]) ) <$> [Steel, CobaltS])
  ++
  ((\m -> ([(12,   10, Bearing m)],        0.5,     Assembler,    [(16, Ball m), (1, Plate m), (10, Lube)]) ) <$> [Titanium, Ceramic])
  )
