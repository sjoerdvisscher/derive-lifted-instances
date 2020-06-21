{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.DeriveLiftedInstances
import Data.Functor.Identity

class Test a where
  op0 :: a
  op1 :: Int -> [a] -> a
  op2 :: [[a]] -> a

instance Test Int where
  op0 = 1
  op1 i is = i + sum is
  op2 = sum . fmap sum

newtype X = X { unX :: Int } deriving Show
mkX :: Int -> X
mkX = X . (`mod` 10)
deriveInstance (isoDeriv [| mkX |] [| unX |] idDeriv) [t| Num X |]

deriveInstance (isoDeriv [| mkX |] [| unX |] idDeriv) [t| Test X |]
deriveInstance (isoDeriv [| mkX |] [| unX |] idDeriv) [t| Eq X |]
deriveInstance (isoDeriv [| mkX |] [| unX |] idDeriv) [t| Ord X |]

deriveInstance showDeriv [t| Test ShowsPrec |]
deriveInstance unitDeriv [t| Test () |]
deriveInstance (apDeriv idDeriv) [t| forall a. Test a => Test [a] |]
deriveInstance (biapDeriv idDeriv idDeriv) [t| forall a b. (Test a, Test b) => Test (a, b) |]
-- deriveInstance (newtypeDeriv 'Identity 'runIdentity idDeriv) [t| forall a. Test a => Test (Identity a) |]

newtype Ap f a = Ap { getAp :: f a } deriving Show
deriveInstance (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv)) [t| forall f a. (Applicative f, Test a) => Test (Ap f a) |]

deriveInstance (newtypeDeriv 'Identity 'runIdentity (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv))) [t| forall f a. (Applicative f, Test a) => Test (Identity (Ap f a)) |]

newtype Id a = Id { runId :: a }
deriveInstance (apDeriv (apDeriv (newtypeDeriv 'Id 'runId idDeriv))) [t| forall a. Test a => Test (() -> Identity (Id a)) |]
deriveInstance (apDeriv (biapDeriv idDeriv idDeriv)) [t| forall a b. (Test a, Test b) => Test (() -> (a, b)) |]
deriveInstance (biapDeriv (apDeriv idDeriv) (newtypeDeriv 'Id 'runId idDeriv)) [t| forall a b. (Test a, Test b) => Test (() -> a, Id b) |]

class Test1 f where
  hop0 :: f a
  hop0' :: f Int
  hop1 :: Int -> f Int -> f Int

instance Test1 [] where
  hop0 = []
  hop0' = [1]
  hop1 i = map (+ i)

deriveInstance (newtypeDeriv 'Ap 'getAp idDeriv) [t| forall f. Test1 f => Test1 (Ap f) |]
deriveInstance (newtypeDeriv 'Ap 'getAp idDeriv) [t| forall f. Functor f => Functor (Ap f) |]
deriveInstance (newtypeDeriv 'Ap 'getAp idDeriv) [t| forall f. Applicative f => Applicative (Ap f) |]
deriveInstance (newtypeDeriv 'Ap 'getAp idDeriv) [t| forall f. Monad f => Monad (Ap f) |]
