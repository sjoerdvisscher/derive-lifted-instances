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

-- instance Test a => Test (() -> Identity a) where
  -- op1 i is = liftA2 (liftA2 id) (liftA2 (liftA2 id) (pure (pure op1)) (pure (pure i))) (fmap (traverse id) . traverse id  $ is)
  -- op2 ass = fmap Identity $ op2 <$> traverse (traverse (fmap runIdentity)) ass

-- instance Test a => Test (() -> a) where
--   op1 i is = op1 <$> pure i <*> traverse id is
--   op2 ass = op2 <$> traverse (traverse id) ass

-- instance (Test a, Test b) => Test (a, b) where
--   op1 i ps = (op1 i (fmap fst ps), op1 i (fmap snd ps))
--   op2 ps = case ((op2, op2), _ ps) of
--     ((f, g), (a, b)) -> (f a, g b)
-- instance Test a => Test (Identity a) where
--   op1 i as = Identity $ op1 i (fmap runIdentity as)
--   op2 ass = Identity $ op2 (fmap (fmap runIdentity) ass)

deriveInstance showDeriv [t| Test ShowsPrec |]
deriveInstance (apDeriv idDeriv) [t| forall a. Test a => Test [a] |]
deriveInstance tupleDeriv [t| forall a b. (Test a, Test b) => Test (a, b) |]
deriveInstance (newtypeDeriv 'Identity 'runIdentity idDeriv) [t| forall a. Test a => Test (Identity a) |]

newtype Ap f a = Ap { getAp :: f a }
deriveInstance (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv)) [t| forall f a. (Applicative f, Test a) => Test (Ap f a) |]

deriveInstance (newtypeDeriv 'Identity 'runIdentity (newtypeDeriv 'Ap 'getAp (apDeriv idDeriv))) [t| forall f a. (Applicative f, Test a) => Test (Identity (Ap f a)) |]

newtype Id a = Id { runId :: a }
deriveInstance (apDeriv (apDeriv (newtypeDeriv 'Id 'runId idDeriv))) [t| forall a. Test a => Test (() -> Identity (Id a)) |]
