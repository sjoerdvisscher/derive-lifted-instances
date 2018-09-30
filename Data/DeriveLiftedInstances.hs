{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveLift, StandaloneDeriving #-}
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
  apDeriv, tupleDeriv, showDeriv, ShowsPrec(..)
) where
  
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import Data.Maybe (fromMaybe)
import Data.Char (isAlpha)
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

deriving instance Lift Fixity
deriving instance Lift FixityDirection
data ShowsPrec = ShowsPrec (Int -> String -> String) | ShowOp2 Fixity (Int -> String -> String) | ShowOp1 Fixity (Int -> String -> String)
instance Show ShowsPrec where
  showsPrec d (ShowsPrec f) = f d
  showsPrec d (ShowOp2 (Fixity p _) f) = showParen (d > p) $ f 0
  showsPrec _ (ShowOp1 _ f) = showParen True $ f 0

showAp :: ShowsPrec -> ShowsPrec -> ShowsPrec
showAp (ShowsPrec f) (ShowsPrec g) = ShowsPrec $ \d -> showParen (d > 10) $ f 11 . showChar ' ' . g 11
showAp (ShowOp2 fx@(Fixity p i) f) (ShowsPrec g) = ShowOp1 fx $ \_ -> g (p + fromEnum (i /= InfixL)) . showChar ' ' . f 0
showAp (ShowOp1 (Fixity p i) f) (ShowsPrec g) = ShowsPrec $ \d -> showParen (d > p) (f 0 . showChar ' ' . g (p + fromEnum (i /= InfixR)))
showAp _ _ = error "Unexpected use of showAp"

showDeriv :: Derivator
showDeriv = noopDeriv {
  op = \nm -> let name = nameBase nm in if isOperator name 
    then do
      fx <- fromMaybe defaultFixity <$> reifyFixity nm
      [|ShowOp2 fx $ const $ showString $(return . LitE . StringL $ name)|]
    else [|ShowsPrec $ const $ showString $(return . LitE . StringL $ name)|],
  ap = \f a -> [|showAp $f $a|],
  arg = \t v -> do
    test <- isClassInstance ''Show t 
    if test then [|ShowsPrec $ flip showsPrec $v|] else [|ShowsPrec $ const (showString "#Unshowable#") |],
  var = \v -> [|ShowsPrec $ flip showsPrec $v|],
  over = \v -> [|ShowsPrec $ flip showsPrec $v|]
} 

isClassInstance :: Name -> Type -> Q Bool
isClassInstance nm t = do
  ClassI _ insts <- reify nm
  return $ any (== t) [ t' | InstanceD _ _ (AppT _ t') _ <- insts]

isOperator :: String -> Bool
isOperator (c:_) = not (isAlpha c) && c /= '_'
isOperator _ = False
