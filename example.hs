{-# OPTIONS_GHC -ddump-splices -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleInstances #-}
import Data.DeriveLiftedInstances
import GHC.Base

-- deriveInstance (apDeriv `composeDeriv` tupleDeriv) [t| forall a b. (Num a, Num b) => Num [(a, b)] |]

-- newtype TwoLists a b = TwoLists { runTwoLists :: ([a], [b]) }
-- deriveInstance 
--   (newtypeDeriv 'TwoLists 'runTwoLists `composeDeriv` tupleDeriv `composeDeriv` apDeriv)
--   [t| forall a b. (Semigroup a, Semigroup b) => Semigroup (TwoLists a b) |]
-- instance (Semigroup a, Semigroup b) => Semigroup (TwoLists a b) where
--     l <> r = TwoLists ((<>) <$> fst (runTwoLists l) <*> fst (runTwoLists r), (<>) <$> snd (runTwoLists l) <*> snd (runTwoLists r))
--     sconcat ts = TwoLists (sconcat <$> (sequnceA . fmap (fst . runTwoLists)) ts, sconcat <$> traverse (snd . runTwoLists) ts)

-- a <> b = RUN $ OP (<>) `AP` VAR a `AP` VAR b
-- sconcat ts = RUN $ OP sconcat `AP` (VAR `OVER` ts)
-- stimes i a = RUN $ ARG i `AP` VAR a

deriveInstance noopDeriv [t| forall a b. (Num a, Num b) => Num (a, b) |]