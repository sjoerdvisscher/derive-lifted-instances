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
  noopDeriv,
  apDeriv, tupleDeriv, newtypeDeriv, showDeriv, ShowsPrec(..)
) where

import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal


apDeriv :: Derivator
apDeriv = noopDeriv {
  op   = \_ o -> [| pure $o |],     -- :: f op
  arg  = \_ e -> [| pure $e |],     -- :: f e
  ap   = \f a -> [| $f <*> $a |],   -- :: f (a -> b) -> f a -> f b
  over = \v   -> [| sequenceA $v |] -- ::
}

tupleDeriv :: Derivator
tupleDeriv = noopDeriv {
  op   = \_ o   -> [| ($o, $o) |],                         -- :: (op, op)
  arg  = \_ e   -> [| ($e, $e) |],                         -- :: (e, e)
  ap   = \fg ab -> [| case ($fg, $ab) of
                        ((f, g), (a, b)) -> (f a, g b) |], -- :: ((a -> c), (b -> d)) -> (a, b) -> (c, d)
  over = \vv    -> [| (fmap fst $vv, fmap snd $vv) |]      -- :: t (a, b) -> (t a, t b)
}

newtypeDeriv :: Name -> Name -> Derivator
newtypeDeriv (pure . ConE -> mk) (pure . VarE -> un) = noopDeriv {
  run  = \e -> [| $mk $e |],     -- a -> N a
  var  = \v -> [| $un $v |],     -- N a -> a
  over = \v -> [| fmap $un $v |] -- t (N a) -> t a
}

deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
