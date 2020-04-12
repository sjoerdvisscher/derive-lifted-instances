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
  apDeriv, tupleDeriv, newtypeDeriv, composeDeriv,
  showDeriv, ShowsPrec(..),
  show1Deriv, ShowsPrec1(..)
) where
  
import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal
import Control.Applicative (liftA2)
import Data.Coerce (coerce)


apDeriv :: Derivator -> Derivator
apDeriv d = d {
  op = \_ o -> [|pure $(op d o)|],
  ap = \f a -> [|liftA2 $(ap d) $f $a|],
  arg = \_ a -> [|pure $(arg d a)|],
  over = \v -> [|sequenceA $v|] -- t (f a) -> f (t a)
}

tupleDeriv :: Derivator
tupleDeriv = apDeriv {
  run = \e -> [|($e fst, $e snd)|],
  var = \v -> [|($ $v)|]
}
-- tupleDeriv = noopDeriv {
--   op = \_ f -> [|($f, $f)|],
--   ap = \app -> [|\(f, g) (a, b) -> ($app f a, $app g b)|],
--   arg = \_ e -> [|($e, $e)|],
--   over = \vv -> [|(fmap fst $vv, fmap snd $vv)|] -- t (a, b) -> (t a, t b)
-- }

newtypeDeriv :: Name -> Name -> Derivator
newtypeDeriv mk un = noopDeriv {
  run = \e -> [|$(pure $ ConE mk) $e|],
  var = \v -> [|$(pure $ VarE un) $v|],
  over = \v -> [|fmap $(pure $ VarE un) $v|] -- t (N a) -> t a
}

composeDeriv :: Derivator -> Derivator -> Derivator
composeDeriv l r = Derivator {
  run = run r . run l,
  op = \nm -> op l nm . op r nm,
  ap = ap l . ap r,
  arg = \t -> arg l t . arg r t,
  var = var l . var r,
  over = over l . over r
}

-- deriveInstance showDeriv [t| Num ShowsPrec |]
-- deriveInstance showDeriv [t| Fractional ShowsPrec |]
-- deriveInstance showDeriv [t| Floating ShowsPrec |]
-- deriveInstance showDeriv [t| Semigroup ShowsPrec |]
-- deriveInstance showDeriv [t| Monoid ShowsPrec |]

-- deriveInstance show1Deriv [t| Functor ShowsPrec1 |]
-- deriveInstance show1Deriv [t| Applicative ShowsPrec1 |]
