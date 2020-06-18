{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.DeriveLiftedInstances
import Data.Functor.Identity
import Data.Functor.Product

class Test a where
  op0 :: a
  op1 :: Int -> [a] -> a
  op2 :: [[a]] -> a

instance Test Int where
  op0 = 1
  op1 i is = i + sum is
  op2 = sum . fmap sum

newtype X = X { unX :: Int } deriving Show
deriveInstance (isoDeriv [| X . (+ 10) |] [| subtract 10 . unX |] idDeriv) [t| Test X |]

deriveInstance showDeriv [t| Test ShowsPrec |]
deriveInstance (apDeriv idDeriv) [t| forall a. Test a => Test [a] |]
deriveInstance (tupleDeriv idDeriv idDeriv) [t| forall a b. (Test a, Test b) => Test (a, b) |]
-- deriveInstance (newtypeDeriv 'Identity 'runIdentity idDeriv) [t| forall a. Test a => Test (Identity a) |]

newtype Ap f a = Ap { getAp :: f a }
deriveInstance (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv)) [t| forall f a. (Applicative f, Test a) => Test (Ap f a) |]

deriveInstance (newtypeDeriv 'Identity 'runIdentity (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv))) [t| forall f a. (Applicative f, Test a) => Test (Identity (Ap f a)) |]

newtype Id a = Id { runId :: a }
deriveInstance (apDeriv (apDeriv (newtypeDeriv 'Id 'runId idDeriv))) [t| forall a. Test a => Test (() -> Identity (Id a)) |]
deriveInstance (apDeriv (tupleDeriv idDeriv idDeriv)) [t| forall a b. (Test a, Test b) => Test (() -> (a, b)) |]
deriveInstance (tupleDeriv (apDeriv idDeriv) (newtypeDeriv 'Id 'runId idDeriv)) [t| forall a b. (Test a, Test b) => Test (() -> a, Id b) |]

class Test1 f where
  hop0 :: f a
  hop0' :: f Int

instance Test1 [] where
  hop0 = []
  hop0' = [1]

deriveInstance (newtypeDeriv 'Ap 'getAp idDeriv) [t| forall f. Test1 f => Test1 (Ap f) |]
