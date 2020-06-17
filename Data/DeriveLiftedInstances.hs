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
  apDeriv, tupleDeriv, newtypeDeriv, showDeriv, ShowsPrec(..)
) where

import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal
import Control.Arrow ((&&&))
import Control.Applicative (liftA2)


apDeriv :: Derivator -> Derivator
apDeriv deriv = deriv {
  run = [| fmap $(run deriv) |],
  op  = \nm o -> [| pure $(op deriv nm o) |],    -- :: f op
  arg = \ty e -> [| pure $(arg deriv ty e) |],    -- :: f e
  ap  = \app -> [| liftA2 $(ap deriv app) |], -- :: (c -> a -> b) -> f c -> f a -> f b
  var = \fold -> [| fmap $(var deriv fold) . $(fold (\v -> [| traverse $v |]) [| id |]) |] -- :: (a -> f b) -> t a -> f (t b)
}

tupleDeriv :: Derivator
tupleDeriv = idDeriv {
  op  = \_ o   -> [| ($o, $o) |],                         -- :: (op, op)
  arg = \_ e   -> [| ($e, $e) |],                         -- :: (e, e)
  ap  = \app -> [| \(f, g) (a, b) -> ($app f a, $app g b) |], -- :: (c -> a -> b) -> (c, c') -> (a, a') -> (b, b')
  var = \fold -> fold (\f -> [| (fmap fst &&& fmap snd) . fmap $f |]) [| id |] -- :: (c -> (a, b)) -> t c -> (t a, t b)
}

newtypeDeriv :: Name -> Name -> Derivator -> Derivator
newtypeDeriv (pure . ConE -> mk) (pure . VarE -> un) deriv = deriv {
  run = [| $mk . $(run deriv) |], -- a -> N a
  var = \fold -> [| $(var deriv fold) . $(fold (\v -> [| fmap $v |]) un) |] -- (b -> a) -> f b -> f a, where b = a, f a, f (g a) etc
}

deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
