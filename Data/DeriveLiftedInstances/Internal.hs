{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DeriveLiftedInstances.Internal
-- Copyright   :  (c) Sjoerd Visscher 2020
--
-- Maintainer  :  sjoerd@w3future.com
-- Stability   :  experimental
-- Portability :  non-portable
-----------------------------------------------------------------------------
module Data.DeriveLiftedInstances.Internal where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Traversable (for)
import Data.Ratio


data Derivator = Derivator {
  run :: Q Exp -> Q Exp,
  op :: Name -> Q Exp -> Q Exp,
  ap :: Q Exp -> Q Exp -> Q Exp,
  arg :: Type -> Q Exp -> Q Exp,
  var :: Q Exp,
  over :: Q Exp -> Q Exp
}

varExp :: Name -> Q Exp
varExp = return . VarE

varPat :: Name -> Q Pat
varPat = return . VarP

idDeriv :: Derivator
idDeriv = Derivator {
  run  = id,
  op   = const id,
  ap   = \f a -> [| $f $a |],
  arg  = const id,
  var  = [| id |],
  over = \v -> [| fmap $v |]
}

deriveInstance :: Derivator -> Q Type -> Q [Dec]
deriveInstance deriv qtyp = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT (ConT className) typeName) ->
      deriveInstance' deriv ctx className typeName
    AppT (ConT className) typeName ->
      deriveInstance' deriv [] className typeName
    _ -> fail $ "No support for type: " ++ show typ

deriveInstance' :: Derivator -> Cxt -> Name -> Type -> Q [Dec]
deriveInstance' deriv ctx className typeName = do
  ClassI (ClassD _ _ [KindedTV tvn _] _ decs) _ <- reify className
  impl <- for decs $ \case
    SigD nm tp -> do
      dec <- reify nm
      case dec of
        ClassOpI{} -> do
          (args, rhs) <- buildOperation deriv tvn tp (op deriv nm (varExp nm))
          body <- run deriv rhs
          return $ Just $ FunD nm [Clause args (NormalB body) []]
        _ -> fail $ "No support for declaration: " ++ show dec
    _ -> return Nothing
  return [InstanceD Nothing ctx (AppT (ConT className) typeName) $ catMaybes impl]

buildOperation :: Derivator -> Name -> Type -> Q Exp -> Q ([Pat], Q Exp)
buildOperation d nm (AppT (AppT ArrowT h) t) e | hasVar nm h = do
  varNm <- newName "var"
  (args, rhs) <- buildOperation d nm t (ap d e [| $(buildArgument d nm h) $(varExp varNm) |])
  return (VarP varNm : args, rhs)
buildOperation d nm (AppT (AppT ArrowT h) t) e = do
  varNm <- newName "arg"
  (args, rhs) <- buildOperation d nm t (ap d e (arg d h (varExp varNm)))
  return (VarP varNm : args, rhs)
buildOperation d nm (ForallT _ _ t) e = buildOperation d nm t e
buildOperation _ nm t e | isVar nm t = return ([], e)
buildOperation _ _ e _ = fail $ "No support for expression: " ++ show e

buildArgument :: Derivator -> Name -> Type -> Q Exp
buildArgument d nm (AppT _ h) = over d (buildArgument d nm h)
buildArgument d nm _ = var d

isVar :: Name -> Type -> Bool
isVar nm (VarT nm') = nm == nm'
isVar nm (AppT h _) = isVar nm h
isVar _ _ = False

hasVar :: Name -> Type -> Bool
hasVar nm (VarT nm') = nm == nm'
hasVar nm (AppT _ h) = hasVar nm h
hasVar _ _ = False

tvName :: TyVarBndr -> Name
tvName (PlainTV nm) = nm
tvName (KindedTV nm _) = nm


deriving instance Lift Fixity
deriving instance Lift FixityDirection
data ShowsPrec = ShowsPrec (Int -> String -> String) | ShowOp2 Fixity (Int -> String -> String) | ShowOp1 Fixity (Int -> String -> String)
instance Show ShowsPrec where
  showsPrec d (ShowsPrec f) = f d
  showsPrec d (ShowOp2 (Fixity p _) f) = showParen (d > p) $ f 0
  showsPrec _ (ShowOp1 _ f) = showParen True $ f 0

showAp :: ShowsPrec -> ShowsPrec -> ShowsPrec
showAp (ShowsPrec f) (ShowsPrec g) = ShowsPrec $ \d -> showParen (d > 10) $ f 10 . showChar ' ' . g 11
showAp (ShowOp2 fx@(Fixity p i) f) (ShowsPrec g) = ShowOp1 fx $ \_ -> g (p + fromEnum (i /= InfixL)) . showChar ' ' . f 0
showAp (ShowOp1 (Fixity p i) f) (ShowsPrec g) = ShowsPrec $ \d -> showParen (d > p) (f 0 . showChar ' ' . g (p + fromEnum (i /= InfixR)))
showAp _ _ = error "Unexpected use of showAp"

showDeriv :: Derivator
showDeriv = idDeriv {
  op = \nm _ -> let name = nameBase nm in if isOperator name
    then do
      fx <- fromMaybe defaultFixity <$> reifyFixity nm
      [| ShowOp2 fx $ const $ showString $(pure . LitE . StringL $ name) |]
    else
      [| ShowsPrec  $ const $ showString $(pure . LitE . StringL $ name) |],
  ap = \f a -> [| showAp $f $a |],
  arg = \case
    (VarT _) -> const [| ShowsPrec $ const (showString "#UnshowableVar#") |]
    _ -> \v -> [| ShowsPrec $ flip showsPrec $v |],
  var  = [| ShowsPrec . flip showsPrec |],
  over = const [| ShowsPrec . flip showsPrec |]
}

isOperator :: String -> Bool
isOperator (c:_) = not (isAlpha c) && c /= '_'
isOperator _ = False
