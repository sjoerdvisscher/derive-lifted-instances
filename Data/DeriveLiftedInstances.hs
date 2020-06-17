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


apDeriv :: Derivator
apDeriv = idDeriv {
  op   = \_ o -> [| pure $o |],    -- :: f op
  arg  = \_ e -> [| pure $e |],    -- :: f e
  ap   = \f a -> [| $f <*> $a |],  -- :: f (a -> b) -> f a -> f b
  over = \v   -> [| traverse $v |] -- :: (a -> f b) -> t a -> f (t b)
}

tupleDeriv :: Derivator
tupleDeriv = idDeriv {
  op   = \_ o   -> [| ($o, $o) |],                         -- :: (op, op)
  arg  = \_ e   -> [| ($e, $e) |],                         -- :: (e, e)
  ap   = \fg ab -> [| case ($fg, $ab) of
                        ((f, g), (a, b)) -> (f a, g b) |], -- :: (a -> c, b -> d) -> (a, b) -> (c, d)
  over = \f     -> [| (fmap fst &&& fmap snd) . fmap $f |] -- :: (c -> (a, b)) -> t c -> (t a, t b)
}

newtypeDeriv :: Name -> Name -> Derivator
newtypeDeriv (pure . ConE -> mk) (pure . VarE -> un) = idDeriv {
  run  = \e -> [| $mk $e |], -- a -> N a
  var  = un,                 -- N a -> a
  over = \v -> [| fmap $v |] -- (b -> a) -> f b -> f a, where b = a, N a, N (N a) etc
}

deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
