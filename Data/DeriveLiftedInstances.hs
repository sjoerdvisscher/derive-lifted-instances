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
  -- * Deriving instances
  deriveInstance,
  -- * Derivators for any class
  idDeriv, newtypeDeriv, isoDeriv,
  -- * Derivators for algebraic classes
  -- $algebraic-classes
  apDeriv, biapDeriv, monoidDeriv,
  showDeriv, ShowsPrec(..),
  -- * Creating derivators
  Derivator(..)
) where

import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal
import Control.Applicative (liftA2)
import Data.Biapplicative

-- $algebraic-classes
-- Algebraic classes are type classes where all the methods return a value of the same type, which is also the class parameter.
-- Examples from base are `Num` and `Monoid`.

-- | Given how to derive an instance for @a@, `apDeriv` creates a `Derivator` for @f a@,
-- when @f@ is an instance of `Applicative`. Example:
--
-- @
-- `deriveInstance` (`apDeriv` `idDeriv`) [t| forall a. `Num` a => `Num` [a] |]
--
-- > [2, 3] `*` [5, 10]
-- [10, 20, 15, 30]
-- @
apDeriv :: Derivator -> Derivator
apDeriv deriv = Derivator {
  res = \v -> [| fmap (\w -> $(res deriv [| w |])) $v |],
  op  = \nm o -> [| pure $(op deriv nm o) |],
  arg = \ty e -> [| pure $(arg deriv ty e) |],
  var = \fold v ->
    [| fmap (\w -> $(var deriv fold [| w |])) ($(fold [| traverse |] [| id |]) $v) |],
  ap  = \f a -> [| liftA2 (\g b -> $(ap deriv [| g |] [| b |])) $f $a |]
}

-- | Given how to derive an instance for @a@ and @b@, `biapDeriv` creates a `Derivator` for @f a b@,
-- when @f@ is an instance of `Biapplicative`. Example:
--
-- @
-- `deriveInstance` (`biapDeriv` `idDeriv` `idDeriv`) [t| forall a b. (`Num` a, `Num` b) => `Num` (a, b) |]
--
-- > (2, 3) `*` (5, 10)
-- (10, 30)
-- @
biapDeriv :: Derivator -> Derivator -> Derivator
biapDeriv l r = Derivator {
  res = \e -> [| bimap (\w -> $(res l [| w |])) (\w -> $(res r [| w |])) $e |],
  op  = \nm o -> [| bipure $(op l nm o) $(op r nm o) |],
  arg = \ty e -> [| bipure $(arg l ty e) $(arg r ty e) |],
  var = \fold v ->
    [| bimap (\w -> $(var l fold [| w |])) (\w -> $(var r fold [| w |]))
       ($(fold [| traverseBia |] [| id |]) $v) |],
  ap  = \f a -> [| biliftA2 (\g b -> $(ap l [| g |] [| b |])) (\g b -> $(ap r [| g |] [| b |])) $f $a |]
}

-- | Create a `Derivator` for any `Monoid` @m@. This is a degenerate instance that only collects
-- all values of type @m@, and ignores the rest.
monoidDeriv :: Derivator
monoidDeriv = idDeriv {
  op  = \_ _ -> [| mempty |],
  arg = \_ _ -> [| mempty |],
  var = \fold v -> [| ($(fold [| foldMap |] [| id |]) $v) |],
  ap  = \f a -> [| $f <> $a |]
}

-- | Given how to derive an instance for @a@, and the names of a newtype wrapper around @a@,
-- `newtypeDeriv` creates a `Derivator` for that newtype. Example:
--
-- @
-- newtype Ap f a = Ap { getAp :: f a } deriving Show
-- `deriveInstance` (`newtypeDeriv` 'Ap 'getAp `idDeriv`) [t| forall f. `Functor` f => `Functor` (Ap f) |]
--
-- > `fmap` (+1) (Ap [1,2,3])
-- Ap {getAp = [2,3,4]}
-- @
newtypeDeriv :: Name -> Name -> Derivator -> Derivator
newtypeDeriv mk un = isoDeriv (pure $ ConE mk) (pure $ VarE un)

-- | Given how to derive an instance for @a@, and two functions of type @a `->` b@ and @b `->` a@,
-- `isoDeriv` creates a `Derivator` for @b@. (Note that the 2 functions don't have to form
-- an isomorphism, but if they don't, the new instance can break the class laws.) Example:
--
-- @
-- newtype X = X { unX :: `Int` } deriving `Show`
-- mkX :: `Int` -> X
-- mkX i = X (`mod` i 10)
-- `deriveInstance` (isoDeriv [| mkX |] [| unX |] `idDeriv`) [t| `Num` X |]
--
-- > mkX 4 `^` 2
-- X {unX = 6}
-- @
isoDeriv :: Q Exp -> Q Exp -> Derivator -> Derivator
isoDeriv mk un deriv = deriv {
  res = \v -> [| $mk $(res deriv v) |],
  var = \fold v -> var deriv fold [| $(fold [| fmap |] un) $v |]
}

deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
