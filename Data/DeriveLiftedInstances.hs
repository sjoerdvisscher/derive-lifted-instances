{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DeriveLiftedInstances
-- Copyright   :  (c) Sjoerd Visscher 2018
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.DeriveLiftedInstances (
  deriveInstance,
  Derivator(..),
  noopDeriv,
  apDeriv, tupleDeriv, showDeriv, ShowsPrec(..)
) where
  
import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal


apDeriv :: Derivator
apDeriv = noopDeriv {
  op = \nm -> [|pure $(varExp nm)|],
  ap = \f a -> [|$f <*> $a|],
  arg = \_ a -> [|pure $a|],
  over = \v -> [|sequenceA $v|]
}

tupleDeriv :: Derivator
tupleDeriv = noopDeriv {
  op = \nm -> [|($(varExp nm), $(varExp nm))|],
  ap = \fg ab -> [|case ($fg, $ab) of ((f, g), (a, b)) -> (f a, g b)|],
  arg = \_ e -> [|($e, $e)|],
  over = \vv -> [|(fmap fst $vv, fmap snd $vv)|]
}

deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]