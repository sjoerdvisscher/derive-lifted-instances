{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Test where

import Data.DeriveLiftedInstances
import Data.Functor.Identity

class Test a where
  op1 :: Int -> [a] -> a
  -- op2 :: [[a]] -> a

instance Test Int where
  op1 i is = i + sum is
  -- op2 = sum . sum

deriveInstance showDeriv [t| Test ShowsPrec |]
deriveInstance apDeriv [t| forall a. Test a => Test [a] |]
deriveInstance tupleDeriv [t| forall a b. (Test a, Test b) => Test (a, b) |]
deriveInstance (newtypeDeriv 'Identity 'runIdentity) [t| forall a. Test a => Test (Identity a) |]
