{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DeriveLiftedInstances
-- Copyright   :  (c) Sjoerd Visscher 2020
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.DeriveLiftedInstances (
  deriveInstance,
  Derivator(..),
  idDeriv,
  apDeriv, tupleDeriv, newtypeDeriv, isoDeriv, showDeriv, ShowsPrec(..)
) where

import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal
import Control.Arrow ((&&&), (***))
import Control.Applicative (liftA2)


apDeriv :: Derivator -> Derivator
apDeriv deriv = deriv {
  run = \v -> [| fmap (\w -> $(run deriv [| w |])) $v |],
  op  = \nm o -> [| pure $(op deriv nm o) |],
  arg = \ty e -> [| pure $(arg deriv ty e) |],
  ap  = \app -> [| liftA2 $(ap deriv app) |],
  var = \fold v ->
    [| fmap (\w -> $(var deriv fold [| w |])) ($(fold (\w -> [| traverse $w |]) [| id |]) $v) |]
}

tupleDeriv :: Derivator -> Derivator -> Derivator
tupleDeriv l r = idDeriv {
  run = \e -> [| ((\w -> $(run l [| w |])) *** (\w -> $(run r [| w |]))) $e |],
  op  = \nm o -> [| ($(op l nm o), $(op r nm o)) |],
  arg = \ty e -> [| ($(arg l ty e), $(arg r ty e)) |],
  ap  = \app  -> [| \(f, g) (a, b) -> ($(ap l app) f a, $(ap r app) g b) |],
  var = \fold v ->
    [| ( $(var l fold [| $(fold (\w -> [| fmap $w |]) [| fst |]) $v |])
       , $(var r fold [| $(fold (\w -> [| fmap $w |]) [| snd |]) $v |])
       ) |]
}

newtypeDeriv :: Name -> Name -> Derivator -> Derivator
newtypeDeriv mk un = isoDeriv (pure $ ConE mk) (pure $ VarE un)

isoDeriv :: Q Exp -> Q Exp -> Derivator -> Derivator
isoDeriv mk un deriv = deriv {
  run = \v -> [| $mk $(run deriv v) |],
  var = \fold v -> var deriv fold [| $(fold (\v -> [| fmap $v |]) un) $v |]
}

deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
