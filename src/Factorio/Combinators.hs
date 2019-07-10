{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Factorio.Combinators where

import           Data.Typeable
import           GHC.TypeLits

data TypedList (a :: [k]) where
  End  :: TypedList '[]
  Cons :: (a ~ (x:xs)) => x -> TypedList xs -> TypedList a

(-<) = Cons
infixr 3 -<

type family Unwrap (a :: [k]) :: [q] where
  Unwrap (x y : xs) = y : Unwrap xs
  Unwrap '[] = '[]

type family UnwrapStar a :: [*] where
  UnwrapStar a = Unwrap a
type family UnwrapNat a :: [Nat] where
  UnwrapNat a = Unwrap a

-- data In (i :: [Nat]) where
--   In :: forall (i :: [Nat]) xs os. (i ~ Unwrap xs, os ~ TypedList xs) => os -> In i

data MaybeN (a :: Nat) where
  Non :: MaybeN a

-- class T (n :: Nat) where
--   i :: Proxy (Unwrap '[MaybeN n] :: [Nat])
--
-- instance T 1 where
--   i = Proxy @[1]
