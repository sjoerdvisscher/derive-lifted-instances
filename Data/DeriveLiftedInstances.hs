{-# LANGUAGE CPP #-}
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
  recordDeriv, apDeriv, biapDeriv, monoidDeriv, monoidDerivBy,
  showDeriv, ShowsPrec(..),
  -- * Creating derivators
  Derivator(..)
) where

import Language.Haskell.TH
import Data.DeriveLiftedInstances.Internal
import Control.Applicative (liftA2)
import Control.Monad (zipWithM)
import Data.Biapplicative
import Data.Reflection

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
monoidDeriv = monoidDerivBy [| (<>) |] [| mempty |]

monoidDerivBy :: Q Exp -> Q Exp -> Derivator
monoidDerivBy append empty = idDeriv {
  op  = \_ _ -> empty,
  arg = \_ _ -> empty,
  var = \fold v -> [| ($(fold [| foldMapBy $append $empty |] [| id |]) $v) |],
  ap  = \f a -> [| $append $f $a |]
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

-- | Given an n-ary function to @a@, and a list of pairs, consisting of a function from @a@ and a
-- `Derivator` for the codomain of that function, create a `Derivator` for @a@. Examples:
--
-- @
-- data Rec f = Rec { getUnit :: f (), getInt :: f Int }
-- deriveInstance
--   (recordDeriv [| Rec |]
--     [ ([| getUnit |], apDeriv monoidDeriv)
--     , ([| getInt  |], apDeriv idDeriv)
--     ])
--   [t| forall f. Applicative f => Num (Rec f) |]
-- @
--
-- @
-- tripleDeriv deriv1 deriv2 deriv3 =
--   recordDeriv [| (,,) |]
--     [ ([| fst3 |], deriv1)
--     , ([| snd3 |], deriv2)
--     , ([| thd3 |], deriv3) ]
-- @
recordDeriv :: Q Exp -> [(Q Exp, Derivator)] -> Derivator
recordDeriv mk flds = Derivator {
  res = \vs -> do vnms <- vars; [| case $vs of $(pat vnms) -> $(exps vnms >>= foldl (\f v -> [| $f $(pure v) |]) mk) |],
  op  = \nm o -> tup $ traverse (\(_, d) -> op d nm o) flds,
  arg = \ty e -> tup $ traverse (\(_, d) -> arg d ty e) flds,
  var = \fold v -> tup $ traverse (\(fld, d) -> var d fold [| $(fold [| fmap |] fld) $v |]) flds,
  ap  = \fs as -> do
    fnms <- funs
    vnms <- vars
    [| case ($fs, $as) of ($(pat fnms), $(pat vnms)) -> $(tup $ zipWithM (\(_, d) (f, v) -> ap d (ex f) (ex v)) flds (zip fnms vnms)) |]
}
  where
    tup :: Q [Exp] -> Q Exp
#if __GLASGOW_HASKELL__ >= 810
    tup = fmap (TupE . fmap Just)
#else
    tup = fmap TupE
#endif
    pat :: [Name] -> Q Pat
    pat = pure . TupP . fmap VarP
    ex :: Name -> Q Exp
    ex = pure . VarE
    exps :: [Name] -> Q [Exp]
    exps = traverse ex
    vars :: Q [Name]
    vars = names "a"
    funs :: Q [Name]
    funs = names "f"
    names :: String -> Q [Name]
    names s = traverse (const (newName s)) flds



deriveInstance showDeriv [t| Bounded ShowsPrec |]
deriveInstance showDeriv [t| Num ShowsPrec |]
deriveInstance showDeriv [t| Fractional ShowsPrec |]
deriveInstance showDeriv [t| Floating ShowsPrec |]
deriveInstance showDeriv [t| Semigroup ShowsPrec |]
deriveInstance showDeriv [t| Monoid ShowsPrec |]
