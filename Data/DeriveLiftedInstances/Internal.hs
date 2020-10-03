{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Data (Data, gmapQl, cast)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Traversable (for)


-- | To write your own `Derivator` you need to show how each part of a method gets lifted.
-- For example, when deriving an instance for type @a@ of the following methods:
--
-- @
-- meth0 :: a
-- meth1 :: Int -> a
-- meth2 :: a -> Either Bool a -> Sum Int
-- meth3 :: Maybe [a] -> IO a
-- @
--
-- the resulting template haskell declarations are (pseudo code):
--
-- @
-- meth0 = $res ($op "meth0" meth0)
-- meth1 a = $res (($op "meth1" meth1) `$ap` ($arg Int a))
-- meth2 ($inp v0) ($inp v1) = $cst (($op "meth2" meth2) `$ap` ($var (`iterate` 0) v0)) `$ap` ($var (`iterate` 1) v1)
-- meth3 ($inp v2) = $eff (($op "meth2" meth2) `$ap` ($var (`iterate` 2) v2))
-- @
data Derivator = Derivator {
  res :: Q Exp -> Q Exp, -- ^ Convert the result of the method
  cst :: Q Exp -> Q Exp, -- ^ Convert the result of the method if it is a constant
  eff :: Q Exp -> Q Exp, -- ^ Convert the result of the method if it has an effect
  op  :: Name -> Q Exp -> Q Exp, -- ^ Convert the method (still unapplied to any arguments)
  arg :: Type -> Q Exp -> Q Exp, -- ^ Convert an argument
  var :: (Q Exp -> Q Exp -> Q Exp) -> Q Exp -> Q Exp, -- ^ Convert a variable
  inp :: Q Pat -> Q Pat, -- ^ Generate an input pattern for a variable
  ap  :: Q Exp -> Q Exp -> Q Exp -- ^ Apply an argument or variable to the method
}

varExp :: Name -> Q Exp
varExp = pure . VarE

-- | The identity `Derivator`. Not useful on its own, but often used as input for other `Derivator`s.
idDeriv :: Derivator
idDeriv = Derivator {
  res = id,
  cst = id,
  eff = id,
  op  = const id,
  arg = const id,
  var = const id,
  inp = id,
  ap  = \f a -> [| $f $a |]
}

-- | Derive the instance with the given `Derivator` and the given instance head.
--
-- The instance head can be passed as a template haskell type quotation, for example:
--
-- @
-- {-\# LANGUAGE TemplateHaskell #-}
-- [t| `Num` `ShowsPrec` |]
-- [t| forall a. `Num` a => `Num` [a] |]
-- [t| forall a b. (`Num` a, `Num` b) => `Num` (a, b) |]
-- @
deriveInstance :: Derivator -> Q Type -> Q [Dec]
deriveInstance deriv qtyp = do
  typ <- qtyp
  case typ of
    ForallT _ ctx (AppT ty typeName) ->
      deriveInstance' deriv ctx (className ty) (AppT ty typeName)
    AppT ty typeName ->
      deriveInstance' deriv [] (className ty) (AppT ty typeName)
    _ -> fail $ "No support for type: " ++ show typ

deriveInstance' :: Derivator -> Cxt -> Name -> Type -> Q [Dec]
deriveInstance' deriv ctx clsName typ = do
  ClassI (ClassD _ _ tvs _ decs) _ <- reify clsName
  #if MIN_VERSION_template_haskell(2,17,0)
  let KindedTV tvn _ _ = last tvs
  #else
  let KindedTV tvn _ = last tvs
  #endif
  impl <- for decs $ \case
    SigD nm tp -> do
      dec <- reify nm
      case dec of
        ClassOpI{} -> do
          (argNames, body) <- buildOperation deriv tvn tp (op deriv nm (varExp nm))
          args <- traverse (\argName -> if contains argName body then (if nameBase argName == "var" then inp deriv else id) . pure . VarP $ argName else pure WildP) argNames
          pure $ Just $ FunD nm [Clause args (NormalB body) []]
        _ -> fail $ "No support for declaration: " ++ show dec
    _ -> pure Nothing
  pure [InstanceD Nothing ctx typ $ catMaybes impl]

buildOperation :: Derivator -> Name -> Type -> Q Exp -> Q ([Name], Exp)
buildOperation d nm (AppT (AppT ArrowT h) t) e | hasVar nm h = do
  varNm <- newName "var"
  (args, rhs) <- buildOperation d nm t (ap d e (var d (buildArgument nm h) (varExp varNm)))
  pure (varNm : args, rhs)
buildOperation d nm (AppT (AppT ArrowT h) t) e = do
  varNm <- newName "arg"
  (args, rhs) <- buildOperation d nm t (ap d e (arg d h (varExp varNm)))
  pure (varNm : args, rhs)
buildOperation d nm (ForallT _ _ t) e = buildOperation d nm t e
buildOperation d nm (AppT _ t) e | isVar nm t = ([],) <$> eff d e
buildOperation d nm t e | isVar nm t = ([],) <$> res d e
                        | otherwise = ([],) <$> cst d e

buildArgument :: Name -> Type -> Q Exp -> Q Exp -> Q Exp
buildArgument nm (AppT h _) _ var | isVar nm h = var
buildArgument nm (AppT _ h) over var = [| $over $(buildArgument nm h over var) |]
buildArgument _ _ _ var = var

isVar :: Name -> Type -> Bool
isVar nm (VarT nm') = nm == nm'
isVar nm (AppT h _) = isVar nm h
isVar _ _ = False

hasVar :: Name -> Type -> Bool
hasVar nm (VarT nm') = nm == nm'
hasVar nm (AppT f a) = isVar nm f || hasVar nm a
hasVar _ _ = False

className :: Type -> Name
className (ConT nm) = nm
className (AppT h _) = className h
className t = error $ "No support for class: " ++ show t

contains :: Data d => Name -> d -> Bool
contains nm = gmapQl (||) False (\d -> maybe (contains nm d) (== nm) $ cast d)


deriving instance Lift Fixity
deriving instance Lift FixityDirection

-- | Helper for showing infix expressions
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

-- | Derive instances for `ShowsPrec`. Example:
--
-- @
-- `deriveInstance` `showDeriv` [t| `Num` `ShowsPrec` |]
--
-- > `show` ((6 `*` 7) `^` 2 :: `ShowsPrec`)
-- "fromInteger 6 * fromInteger 7 * (fromInteger 6 * fromInteger 7)"
-- @
showDeriv :: Derivator
showDeriv = idDeriv {
  op = \nm _ -> let name = nameBase nm in if isOperator name
    then do
      fx <- fromMaybe defaultFixity <$> reifyFixity nm
      [| ShowOp2 fx $ const $ showString $(pure . LitE . StringL $ name) |]
    else
      [| ShowsPrec  $ const $ showString $(pure . LitE . StringL $ name) |],
  arg = \case
    (VarT _) -> const [| ShowsPrec $ const (showString "#Unshowable#") |]
    _ -> \v -> [| ShowsPrec $ flip showsPrec $v |],
  var = \_ v ->  [| ShowsPrec $ flip showsPrec $v |],
  ap  = \f a -> [| showAp $f $a |]
}

isOperator :: String -> Bool
isOperator (c:_) = not (isAlpha c) && c /= '_'
isOperator _ = False
