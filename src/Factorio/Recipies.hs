{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Factorio.Recipies where

import           Control.Arrow         (first, second, (&&&), (***))
import           Data.List             (delete, intercalate, minimumBy, sortBy)
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

newtype Time = Time { time :: Double } deriving (Show, Eq, Ord)
newtype Drain = Drain { drain :: Double} deriving (Eq, Show, Ord)

data FType = RocketLaunch | Silo | Assembler | Refinery | Chemical | Pump  | Furnace | Mining | Burner | Boiler
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
    pIngredients :: [Ingredient],
    pSummarize   :: Bool
  } deriving (Eq, Ord)
instance Show Process where
  show p = [show.pIngredients, const " → ", show.pProduct] >>= ($ p)

data Factory = Factory {
    fMachine :: Machine,
    fProc    :: Process
  } deriving (Eq, Show, Ord)

showFac :: (Factory, Int) -> String
showFac (f, i) = printf "% 4dx % 12s  {%s} ← {%s}" i (show $ fMachine f) outs ins where
  d    = fromIntegral i :: Double
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
  products = pProduct
  ingredients = pIngredients

instance Production Factory where
  products =  products . fProc
  ingredients = ingredients . fProc

class ToDouble n where
  toDouble :: n -> Double
instance ToDouble Double where
  toDouble = id
instance ToDouble Int where
  toDouble = fromIntegral
instance ToDouble RecursiveFactory where
  toDouble = rfCt

class ProductionRate p  where
    needRate :: p -> ItemCounts
    productionRate :: p -> ItemCounts
    netRate :: p -> ItemCounts
    netRate p = productionRate p - needRate p

instance ProductionRate Factory where
  productionRate (Factory m pr) = let
    -- FType Int Drain Productivity SpeedFactor
    (Machine _ _ _ (Productivity p) (SpeedFactor s)) = m
    (Process (Time t) _ ps _ _) = pr
    in M.fromList $ map (\x -> (itm x, (1+p) * amt x * s / t)) ps

  needRate (Factory m pr) = let
    -- FType Int Drain Productivity SpeedFactor
    (Machine _ _ _ _ (SpeedFactor s)) = m
    (Process (Time t) _ _ is _) = pr
    in M.fromList $ map (\x -> (itm x, amt x * s / t)) is

instance (ToDouble n) => ProductionRate (M.Map Factory n) where
  productionRate fs = sum $ uncurry (*:) <$> ((productionRate *** toDouble) <$> M.toList fs)
  needRate fs = sum $ uncurry (*:) <$> ((needRate *** toDouble) <$> M.toList fs)

data RecursiveFactory = RecursiveFactory {
    rfFactory  :: Factory,
    rfCt       :: Double,
    rfChildren :: M.Map Factory RecursiveFactory
  }

shouldSummarize :: RecursiveFactory -> Bool
shouldSummarize = pSummarize . fProc . rfFactory

showRecursiveFactories :: [RecursiveFactory] -> String
showRecursiveFactories roots = printf "%s\n ---\n%s" (intercalate "\n" $ sh Nothing 0 <$> roots) summarize where
  smallRatio a b
    | a < b = minAbsFst $ sr a b
    | a > b = (\(x,y) -> (y,x)) $ minAbsFst $ sr b a
    | otherwise = (1,1)
    where
      minAbsFst = snd . minimumBy (comparing (abs.fst)) . filter ((< 0.01).fst)
      sr a b = [ (a/b - fromIntegral x/fromIntegral  y, (x,y) :: (Int, Int)) | x <- [1..9], y <- [1..100]]
  onlySum = filter shouldSummarize . M.elems . rfChildren
  notSum  = filter (not.shouldSummarize) . M.elems . rfChildren
  showR :: ItemCounts -> String
  showR m = intercalate ", " ((\(i, c) -> printf "(%s %.2f/s)" (show i) c) <$> M.toList m)
  xCt :: Double -> String
  xCt d
    | d < 1     = printf "%.3f" d
    | d < 10    = printf "%.1f" d
    | otherwise = printf "%d" (ceiling d :: Int)
  showFacFrac :: Maybe Double -> (Factory, Double) -> String
  showFacFrac Nothing (f, d) = printf "%sx %s %s" (xCt d) (show $ fMachine f) outs where
    outs = showR  $ productionRate f *: d

  showFacFrac (Just parentCt) (f, d) = printf "|%s%sx %s %s" ratio (xCt d) (show $ fMachine f) outs where
    outs = showR  $ productionRate f *: d
    ratio
      | parentCt <= 1    = ""
      | d        <= 1    = ""
      | cr > ceiling d   = ""
      | otherwise        = printf "%d:%d " cr pr
      where (cr, pr) = smallRatio d parentCt
  sh :: Maybe Double -> Int -> RecursiveFactory -> String
  sh pCt level rf@(RecursiveFactory f ct children) = let
    leadingSpaces = replicate (level * 2) ' '
    myShow = showFacFrac pCt (f,ct) ++ shortsumChildren
    showChildren = sh (Just ct) (level+1) <$> notSum rf
    shortsumChildren  = case (showR . sum) $ productionRate <$> onlySum rf of
                           [] -> ""
                           s  -> " ← {"++s++"}"
    in leadingSpaces ++ intercalate "\n" (myShow : showChildren)
  addToRfCt :: RecursiveFactory -> RecursiveFactory -> RecursiveFactory
  addToRfCt (RecursiveFactory f ct ch) b = RecursiveFactory f (ct + rfCt b) ch
  summarize = let
    findS :: RecursiveFactory -> [RecursiveFactory]
    findS rf = onlySum rf ++ ((M.elems . rfChildren) rf >>= findS)
    allSum = M.elems $ M.fromListWith addToRfCt $ (rfFactory &&& id) <$> (roots >>= findS)
    in intercalate "\n" $ sh Nothing 0 <$> allSum

instance Production RecursiveFactory where
  products (RecursiveFactory f _ _) = products f
  ingredients (RecursiveFactory f _ _) = ingredients f

instance ProductionRate RecursiveFactory where
  needRate (RecursiveFactory f ct ch) = needRate ch + needRate f *: ct
  productionRate (RecursiveFactory f ct ch) = productionRate ch + productionRate f *: ct

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

calculateNeededFactoriesFrac :: (ProductionRate f) => f -> Item -> Double -> Double
calculateNeededFactoriesFrac f i r = let
  rawRate = fromJust $ M.lookup i $ productionRate f
  in r / rawRate


-- calculateNeededFactories :: Factory -> Item -> Double -> Int
-- calculateNeededFactories f i r = ceiling $ calculateNeededFactoriesFrac f i r

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

instance (Ord a, Fractional n) => Fractional (M.Map a n) where
  fromRational = error "no sane value for a map with no key"
  (/) = merge dropMissing dropMissing (zipWithMatched $ const (/))

class Num n => InternalNum m n where
  internalNumOp :: (n -> n) -> m -> m
  (*:) :: m -> n -> m
  (*:) m n = internalNumOp (* n) m
  (+:) :: m -> n -> m
  (+:) m n = internalNumOp (+ n) m
  (-:) :: m -> n -> m
  (-:) m n = internalNumOp (flip (-) n) m
infixl 7 *:
infixl 6 +:
infixl 6 -:

instance (Ord k, Num n) => InternalNum (M.Map k n) n where
  internalNumOp = M.map
instance InternalNum RecursiveFactory Double where
  internalNumOp f (RecursiveFactory fa ct ch) = RecursiveFactory fa (f ct) ch


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

-- recursiveSolver :: ItemCounts -> M.Map Factory RecursiveFactory
-- recursiveSolver intialNeeds = let
--   toRec :: (Item, Double) -> RecursiveFactory
--   toRec (item, rate) = let
--     myFactory = preferedFactory item
--     neededFactories = calculateNeededFactoriesFrac myFactory item rate
--     myNeeds = abs $ M.filter (< -0.001) $ netRate myFactory *: neededFactories
--     myChildren = recursiveSolver myNeeds
--     in RecursiveFactory myFactory neededFactories myChildren
--   needsList = M.toList intialNeeds
--   results = toRec <$> needsList
--   rfToTuple r = (rfFactory r, r)
--   in M.fromList $ rfToTuple <$> results

recursiveSolver :: ItemCounts -> M.Map Factory RecursiveFactory
recursiveSolver = go where
  go needs = M.mapWithKey goRec $ itr needs M.empty
  goRec f ct = RecursiveFactory f ct $ go $ needRate f *: ct
  itr :: ItemCounts -> FactoryFloat -> FactoryFloat
  itr needs factories
    | haveAll   = factories
    | otherwise = itr needs factories'
    where
      myRate           = productionRate factories :: ItemCounts
      remainingNeeds   = M.filter (> 0) (needs - myRate)
      firstNeeedType   = preferedFactory $ head (M.keys remainingNeeds)
      haveAll          = M.null remainingNeeds
      oneChild         = goRec firstNeeedType 1
      childAmountToAdd = maximum $ M.elems $ remainingNeeds / productionRate oneChild :: Double
      addSome Nothing  = Just childAmountToAdd
      addSome (Just d) = Just $ d + childAmountToAdd
      factories'       = M.alter addSome firstNeeedType factories


iterativeSolver :: ItemCounts -> FactoryCounts
iterativeSolver initialNeeds = let
  iToF = preferedFactory
  needF :: FactoryFloat -> ItemCounts
  needF fs = abs $ M.filter (< -0.001) $ netRate fs - initialNeeds
  itr :: FactoryFloat -> FactoryFloat
  itr fs
    | haveAll   = fs
    | otherwise = itr (fs + nFacs)
    where
      needs     = needF fs
      firstNeed = head $ M.keys needs
      firstRate = needs M.! firstNeed
      nFacs     = let
        f = iToF firstNeed
        n = calculateNeededFactoriesFrac f firstNeed firstRate
        -- in M.singleton f (max 1 n)
        in M.singleton f 0.01
      haveAll   = M.null needs
  in M.map ceiling $ itr M.empty

{- Game Data -}

data Material = Iron | Copper | Steel
  deriving (Eq, Show, Ord)

data TechTier     = Green | Yellow | Red | Blue deriving (Eq, Show, Ord)
data InserterType = Simple | Filter | Stack | StackFilter deriving (Eq, Show, Ord)
data Warhead      = AP | HE deriving (Eq, Show, Ord)
data Mk           = Mk1 | Mk2 | Mk3  deriving (Eq, Show, Ord)
data Mod          = Speed | Prod | Efficiency deriving (Eq, Show, Ord)
data BotType      = Logistics | Construction deriving (Eq, Show, Ord)

data Item =
  YellowSci | GreenSci | RedSci | BlueSci | PurpleSci | MilSci | SpaceSci |
  RCU | RocketPart | Satelite | Rocket |
  BotFrame | Bot BotType | LowDensity |
  EMotor | Engine | Lube |
  Circuit TechTier | Wire |
  Inserter | Belt |
  Explosive | Plastic | Battery | Acid | Sulphur | RocketFuel | SolidFuel |
  CrudeOil | HeavyOil | LightOil | PetGas | Coal |
  Rail | EFurnace | Module Mk Mod |
  Gear | IronStick | Pipe | PipeBend |
  Brick | Concrete | ReinforcedConcrete |
  Magazine Mk | Grenade Mk | Wall | CannonShell Warhead | Radar |
  Accumulator | SolarPanel |
  Ore Material   | Plate Material |
  Mj | Steam |
  Stone |  Water

  deriving (Eq, Show, Ord)

machines :: [Machine]
machines = ( (\(t, g, d, s) -> Machine t g (Drain d) (Productivity 0) (SpeedFactor s)) <$> [
    -- (Assembler, 1, 100 + 3.3, 0.5),
    -- (Assembler, 2, 135 + 4.5, 0.75),
    (Assembler, 3, 210 + 7, 1.25),
    (Refinery, 1, 420 + 14, 1.6),
    (Pump, 1, 10, 1),
    (Chemical, 1, 259 + 8.6, 1),
    (Furnace, 3, 180 + 6, 2),
    (Burner, 1, 0, 1),
    (Boiler, 1, 0, 1),
    (RocketLaunch, 1, 0, 1)
  ])
  ++ [
    Machine Silo   1 (Drain 100) (Productivity 0.4) (SpeedFactor 0.6),
    Machine Mining 1 (Drain 160) (Productivity 0.2) (SpeedFactor 0.5)
  ]

processes :: [Process]
processes = (\(ps, t, f, is, sumarize) -> Process (Time t) f ((\(a,b,c) -> Product a b c) <$> ps) (uncurry Ingredient <$> is) sumarize) <$>
  ([
-- ([(, )], 1, Chemical, [(, ), (, )], False),
  -- ([( 12, 10,  HeavyOil)],                2.5,   Chemical,     [(2, Coal), (15, Water)], False),

  ([( 3, 10,  YellowSci)],              21,   Assembler,   [(2, Circuit Blue), (1, BotFrame), (3, LowDensity)], False),
  ([( 3, 10,  PurpleSci)],              21,   Assembler,   [(30, Rail), (1, EFurnace), (1, Module Mk1 Prod)], False),
  ([( 1, 10,  RedSci)],                  5,   Assembler,   [(1, Plate Copper), (1, Gear)], False),
  ([( 1, 10,  GreenSci)],                6,   Assembler,   [(1, Inserter), (1, Belt)], False),
  ([(1000, 10,SpaceSci)],                1,   RocketLaunch,[(1, Rocket)], False),

  ([( 1, 10,  Rocket)],                 60,   RocketLaunch,[(100, RocketPart), (1, Satelite)], False),

  ([( 1, 10,  RocketPart)],              3,   Silo,        [(10, RCU), (10, LowDensity), (10, RocketFuel)], False),
  ([( 1, 10,  Satelite)],                3,   Assembler,   [(100, Circuit Blue), (100, LowDensity), (50, RocketFuel), (100, SolarPanel), (100, Accumulator), (5, Radar)], False),
  ([( 1, 10,  RCU)],                    30,   Assembler,   [(1, Module Mk1 Speed), (1, Circuit Blue)], False),

  ([( 1, 10,  BotFrame)],               20,   Assembler,   [(3, Circuit Green), (1, EMotor), (1, Plate Steel), (2, Battery)], False),
  ([( 1, 10,  EMotor)],                 10,   Assembler,   [(2, Circuit Green), (1, Engine), (15, Lube)], False),
  ([( 1, 10,  Engine)],                 10,   Assembler,   [(1, Plate Steel), (1, Gear), (2, Pipe)], False),
  ([( 1, 10,  LowDensity)],             20,   Assembler,   [(20, Plate Copper), (2, Plate Steel), (5, Plastic)], False),

  ([( 1, 10,  Module Mk1 Prod)],        15,   Assembler,   [(5, Circuit Green), (5, Circuit Red)], False),
  ([( 1, 10,  Module Mk1 Speed)],       15,   Assembler,   [(5, Circuit Green), (5, Circuit Red)], False),
  ([( 1, 10,  EFurnace)],                5,   Assembler,   [(10, Plate Steel), (5, Circuit Red), (10, Brick)], False),
  ([( 2, 10,  Rail)],                  0.5,   Assembler,   [(1, Plate Steel), (1, Stone), (1, IronStick)], False),

  ([( 2, 10,  Inserter)],              0.5,   Assembler,   [(1, Plate Iron), (1, Circuit Green), (1, Gear)], False),
  ([( 2, 10,  Belt)],                  0.5,   Assembler,   [(1, Plate Iron), (1, Gear)], False),

  ([( 1, 10,  Circuit Green)],         0.5,   Assembler,   [(1, Plate Iron), (3, Wire)], False),
  ([( 1, 10,  Circuit Red)],             6,   Assembler,   [(2, Plastic), (4, Wire), (2, Circuit Green)], False),
  ([( 1, 10,  Circuit Blue)],           10,   Assembler,   [(5, Acid), (2, Circuit Red), (20, Circuit Green)], False),

  ([( 1, 10,  Accumulator)],            10,   Assembler,   [(2, Plate Iron), (5, Battery)], False),
  ([( 1, 10,  SolarPanel)],             10,   Assembler,   [(5, Plate Steel), (5, Plate Copper), (15, Circuit Green)], False),

  ([( 1, 10,  Battery)],                 4,   Chemical,    [(1, Plate Iron), (1, Plate Copper), (20, Acid)], False),
  ([( 2, 10,  Plastic)],                 1,   Chemical,    [(20, PetGas), (1, Coal)], True),
  ([( 2, 10,  Sulphur)],                 1,   Chemical,    [(30, PetGas), (30, Water)], False),
  ([( 50, 10,  Acid)],                   1,   Chemical,    [(1, Plate Iron), (5, Sulphur), (100, Water)], True),
  ([( 10, 10, Lube)],                    1,   Chemical,    [(10, HeavyOil)], True),

  ([( 1, 10,  SolidFuel)],               2,   Chemical,    [(10, LightOil)], False),
  ([( 1, 10,  RocketFuel)],             30,   Chemical,    [(10, LightOil), (10, SolidFuel)], True),

  ([( 2, 10,  Pipe)],                  0.5,   Assembler,   [(1, Plate Iron)], False),
  ([( 1, 10,  Gear)],                  0.5,   Assembler,   [(2, Plate Iron)], False),
  ([( 2, 10,  IronStick)],             0.5,   Assembler,   [(1, Plate Iron)], False),
  ([( 2, 10,  Wire)],                  0.5,   Assembler,   [(1, Plate Copper)], False),
  ([( 1, 10,  Wall)],                  0.5,   Assembler,   [(5, Brick)], False),
  ([( 1, 10,  Radar)],                 0.5,   Assembler,   [(10, Plate Iron), (5, Gear), (5, Circuit Green)], False),

  ([( 1, 10,  Plate Copper)],          3.2,   Furnace,     [(1, Ore Copper)], True),
  ([( 1, 10,  Plate Iron)],            3.2,   Furnace,     [(1, Ore Iron)], True),
  ([( 1, 10,  Plate Steel)],            16,   Furnace,     [(5, Plate Iron)], True),
  ([( 1, 10,  Brick)],                 3.2,   Furnace,     [(2, Stone)], False),
  ([( 10, 10, Concrete)],              3.2,   Assembler,   [(5, Brick), (1, Ore Iron), (100, Water)], False),

  ([( 1, 10,  Ore Copper)],              1,   Mining,     [], True),
  ([( 1, 10,  Ore Iron)],                1,   Mining,     [], True),
  ([( 1, 10,  Coal)],                    1,   Mining,     [], True),
  ([( 1, 10,  Stone)],                   1,   Mining,     [], True),

  ([( 20, 10,  PetGas)],                 2,   Chemical,    [(30, LightOil), (30, Water)], False),
  ([( 30, 10,  LightOil)],               2,   Chemical,    [(40, HeavyOil), (30, Water)], False),
  ([( 25, 30,  HeavyOil), ( 45, 30,  LightOil), ( 55, 30,  PetGas)], 5,   Refinery,     [(100, CrudeOil), (50, Water)], False),
  ([( 65, 20,  HeavyOil), ( 20, 20,  LightOil), ( 10, 20,  PetGas)], 5,   Refinery,     [(10, Coal), (50, Steam)], False),

  ([( 60, 10, Steam)],                  1,   Boiler,      [(0.45, Coal), (60, Water)], False),

  ([(25, 10,  CrudeOil)],               1,   Pump,     [], True),
  ([(1200, 10,  Water)],                1,   Pump,     [], True)

  ])
