{-# LANGUAGE DataKinds              #-}
-- {-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Factorio.Combinators where


data Io = I | O
data CType = Arith | Decide
data MultiGroup = Ea | Al
data Pairing = Paired | Unpaired
data Multi (t :: CType) (i :: Io) (p :: Pairing) (g :: MultiGroup) where
  Each :: Multi t i 'Paired 'Ea
  All  :: Multi 'Decide i p 'Al
  Any  :: Multi 'Decide 'I 'Paired 'Al
data Signal (t :: CType) (i :: Io) (p :: Pairing) (g :: MultiGroup) where
  Const  :: Int -> Signal t 'I 'Unpaired g
  Signal :: String -> Signal t i 'Unpaired g
  MultiS :: Multi t i p g -> Signal t i p g
  OutOne :: Signal 'Decide 'O p g -> Signal 'Decide 'O p g
data Op (t :: CType) where
  Add  :: Op 'Arith
  Sub  :: Op 'Arith
  Mul  :: Op 'Arith
  Div  :: Op 'Arith
  Mod  :: Op 'Arith
  LSh  :: Op 'Arith
  RSh  :: Op 'Arith
  Exp  :: Op 'Arith
  BAnd :: Op 'Arith
  BOr  :: Op 'Arith
  BXor :: Op 'Arith
  Gt   :: Bool -> Op 'Decide
  Lt   :: Bool -> Op 'Decide
  Equ  :: Op 'Decide
  Neq  :: Op 'Decide



data Combinator where
  Combinator ::   Signal t 'I p g
               -> Op t
               -> (forall x. Signal t 'I 'Unpaired x)
               -> Signal t 'O p g
               -> Combinator


test :: Combinator
test = Combinator (Signal "iron") Equ (Const 3) (MultiS All)
