{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.Meta.Translate (toExp) where

#if MIN_VERSION_ghc(9,2,0)
import GHC.Hs.Type (HsWildCardBndrs (..), HsType (..), HsSigType(HsSig), sig_body)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Hs.Type (HsWildCardBndrs (..), HsType (..), HsImplicitBndrs(HsIB), hsib_body)
#elif MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Types (HsWildCardBndrs (..), HsType (..), HsImplicitBndrs (HsIB, hsib_body))
#else
import HsTypes (HsWildCardBndrs (..), HsType (..), HsImplicitBndrs (HsIB), hsib_body)
#endif

#if MIN_VERSION_ghc(9,6,0)
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
import GHC.Hs.Pat as Pat
import GHC.Hs.Lit
#else
import HsExpr as Expr
import HsExtension as Ext
import HsPat as Pat
import HsLit
#endif

import qualified Data.ByteString as B
import qualified Language.Haskell.TH.Syntax as GhcTH
import qualified Language.Haskell.TH.Syntax as TH

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.SrcLoc
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Data.FastString
#if MIN_VERSION_ghc(9,2,0)
import GHC.Utils.Outputable (ppr)
import GHC.Types.Basic (Boxity(..))
import GHC.Types.SourceText (il_value, rationalFromFractionalLit)
import GHC.Driver.Ppr (showSDoc)
#else
import GHC.Utils.Outputable (ppr, showSDoc)
import GHC.Types.Basic (il_value, fl_value, Boxity(..))
#endif
import GHC.Driver.Session (DynFlags, xopt_set, defaultDynFlags)
import qualified GHC.Unit.Module as Module
#else
import SrcLoc
import Name
import RdrName
import FastString
import Outputable (ppr, showSDoc)
import BasicTypes (il_value, fl_value, Boxity(..))
import DynFlags (DynFlags, xopt_set, defaultDynFlags)
import qualified Module
#endif

import GHC.Stack
import qualified Language.Haskell.Meta.Settings as Settings

import qualified Data.List.NonEmpty as NonEmpty

#if MIN_VERSION_ghc(9,2,0)
-- TODO: why this disapears in GHC >= 9.2?
fl_value = rationalFromFractionalLit
#endif



-----------------------------

toLit :: HsLit GhcPs -> TH.Lit
toLit (HsChar _ c) = TH.CharL c
toLit (HsCharPrim _ c) = TH.CharPrimL c
toLit (HsString _ s) = TH.StringL (unpackFS s)
toLit (HsStringPrim _ s) = TH.StringPrimL (B.unpack s)
toLit (HsInt _ i) = TH.IntegerL (il_value i)
toLit (HsIntPrim _ i) = TH.IntPrimL i
toLit (HsWordPrim _ i) = TH.WordPrimL i
toLit (HsInt64Prim _ i) = TH.IntegerL i
toLit (HsWord64Prim _ i) = TH.WordPrimL i
toLit (HsInteger _ i _) = TH.IntegerL i
toLit (HsRat _ f _) = TH.FloatPrimL (fl_value f)
toLit (HsFloatPrim _ f) = TH.FloatPrimL (fl_value f)
toLit (HsDoublePrim _ f) = TH.DoublePrimL (fl_value f)
#if MIN_VERSION_ghc(9,8,0)
toLit HsInt8Prim{} = noTH "toLit" "HsInt8Prim"
toLit HsInt16Prim{} = noTH "toLit" "HsInt16Prim"
toLit HsInt32Prim{} = noTH "toLit" "HsInt32Prim"
toLit HsWord8Prim{} = noTH "toLit" "HsWord8Prim"
toLit HsWord16Prim{} = noTH "toLit" "HsWord16Prim"
toLit HsWord32Prim{} = noTH "toLit" "HsWord32Prim"
#endif
#if !MIN_VERSION_ghc(9,0,0)
toLit (XLit _) = noTH "toLit" "XLit"
#endif

toLit' :: OverLitVal -> TH.Lit
toLit' (HsIntegral i) = TH.IntegerL (il_value i)
toLit' (HsFractional f) = TH.RationalL (fl_value f)
toLit' (HsIsString _ fs) = TH.StringL (unpackFS fs)

toType :: HsType GhcPs -> TH.Type
toType (HsWildCardTy _) = TH.WildCardT
toType (HsTyVar _ _ n) =
  let n' = unLoc n
   in if isRdrTyVar n'
        then TH.VarT (toName n')
        else TH.ConT (toName n')
toType t = todo "toType" (showSDoc(Settings.baseDynFlags []) . ppr $ t)

toName :: RdrName -> TH.Name
toName n = case n of
  (Unqual o) -> TH.mkName (occNameString o)
  (Qual m o) -> TH.mkName (Module.moduleNameString m <> "." <> occNameString o)
  (Orig _ _) -> error "orig"
  (Exact _) -> error "exact"

toFieldExp :: a
toFieldExp = undefined

toPat :: DynFlags -> Pat.Pat GhcPs -> TH.Pat
toPat _dynFlags (Pat.VarPat _ (unLoc -> name)) = TH.VarP (toName name)
toPat dynFlags p = todo "toPat" (showSDoc dynFlags . ppr $ p)

toExp :: DynFlags -> Expr.HsExpr GhcPs -> TH.Exp
toExp _ (Expr.HsVar _ n) =
  let n' = unLoc n
   in if isRdrDataCon n'
        then TH.ConE (toName n')
        else TH.VarE (toName n')

#if MIN_VERSION_ghc(9,0,0)
toExp _ (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString . occName $ n)
#else
toExp _ (Expr.HsUnboundVar _ n)              = TH.UnboundVarE (TH.mkName . occNameString . Expr.unboundVarOcc $ n)
#endif

toExp _ Expr.HsIPVar {}
  = noTH "toExp" "HsIPVar"

toExp _ (Expr.HsLit _ l)
  = TH.LitE (toLit l)

toExp _ (Expr.HsOverLit _ OverLit {ol_val})
  = TH.LitE (toLit' ol_val)

toExp d (Expr.HsApp _ e1 e2)
  = TH.AppE (toExp d . unLoc $ e1) (toExp d . unLoc $ e2)

#if MIN_VERSION_ghc(9,2,0)
#if MIN_VERSION_ghc(9,6,0) && !MIN_VERSION_ghc(9,10,0)
toExp d (Expr.HsAppType _ e _ HsWC {hswc_body}) = TH.AppTypeE (toExp d . unLoc $ e) (toType . unLoc $ hswc_body)
#else
toExp d (Expr.HsAppType _ e HsWC {hswc_body}) = TH.AppTypeE (toExp d . unLoc $ e) (toType . unLoc $ hswc_body)
#endif
toExp d (Expr.ExprWithTySig _ e HsWC{hswc_body=unLoc -> HsSig{sig_body}}) = TH.SigE (toExp d . unLoc $ e) (toType . unLoc $ sig_body)
#elif MIN_VERSION_ghc(8,8,0)
toExp d (Expr.HsAppType _ e HsWC {hswc_body}) = TH.AppTypeE (toExp d . unLoc $ e) (toType . unLoc $ hswc_body)
toExp d (Expr.ExprWithTySig _ e HsWC{hswc_body=HsIB{hsib_body}}) = TH.SigE (toExp d . unLoc $ e) (toType . unLoc $ hsib_body)
#else
toExp d (Expr.HsAppType HsWC {hswc_body} e) = TH.AppTypeE (toExp d . unLoc $ e) (toType . unLoc $ hswc_body)
#endif

toExp d (Expr.OpApp _ e1 o e2)
  = TH.UInfixE (toExp d . unLoc $ e1) (toExp d . unLoc $ o) (toExp d . unLoc $ e2)

toExp d (Expr.NegApp _ e _)
  = TH.AppE (TH.VarE 'negate) (toExp d . unLoc $ e)

-- NOTE: for lambda, there is only one match
#if MIN_VERSION_ghc(9,10,0)
toExp d (Expr.HsLam _ LamSingle (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)]))))
#elif MIN_VERSION_ghc(9,6,0)
toExp d (Expr.HsLam _ (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)]))))
#else
toExp d (Expr.HsLam _ (Expr.MG _ (unLoc -> (map unLoc -> [Expr.Match _ _ (map unLoc -> ps) (Expr.GRHSs _ [unLoc -> Expr.GRHS _ _ (unLoc -> e)] _)])) _))
#endif
  = TH.LamE (fmap (toPat d) ps) (toExp d e)

-- toExp (Expr.Let _ bs e)                       = TH.LetE (toDecs bs) (toExp e)
--
#if MIN_VERSION_ghc(9, 0, 0)
toExp d (Expr.HsIf _ a b c)                   = TH.CondE (toExp d (unLoc a)) (toExp d (unLoc b)) (toExp d (unLoc c))
#else
toExp d (Expr.HsIf _ _ a b c)                   = TH.CondE (toExp d (unLoc a)) (toExp d (unLoc b)) (toExp d (unLoc c))
#endif

-- toExp (Expr.MultiIf _ ifs)                    = TH.MultiIfE (map toGuard ifs)
-- toExp (Expr.Case _ e alts)                    = TH.CaseE (toExp e) (map toMatch alts)
-- toExp (Expr.Do _ ss)                          = TH.DoE (map toStmt ss)
-- toExp e@Expr.MDo{}                            = noTH "toExp" e
--
#if MIN_VERSION_ghc(9, 2, 0)
toExp d (Expr.ExplicitTuple _ args boxity) = ctor tupArgs
#else
toExp d (Expr.ExplicitTuple _ (map unLoc -> args) boxity) = ctor tupArgs
#endif
  where
    toTupArg (Expr.Present _ e) = Just $ unLoc e
    toTupArg (Expr.Missing _) = Nothing
    toTupArg _ = error "impossible case"

    ctor = case boxity of
      Boxed -> TH.TupE
      Unboxed -> TH.UnboxedTupE

#if MIN_VERSION_ghc(8,10,0)
    tupArgs = fmap ((fmap (toExp d)) . toTupArg) args
#else
    tupArgs = case traverse toTupArg args of
      Nothing -> error "Tuple section are not supported by template haskell < 8.10"
      Just args -> fmap (toExp d) args
#endif

-- toExp (Expr.List _ xs)                        = TH.ListE (fmap toExp xs)
#if MIN_VERSION_ghc(9, 4, 0) && !MIN_VERSION_ghc(9, 10, 0)
toExp d (Expr.HsPar _ _ e _)
#else
toExp d (Expr.HsPar _ e)
#endif
  = TH.ParensE (toExp d . unLoc $ e)

toExp d (Expr.SectionL _ (unLoc -> a) (unLoc -> b))
  = TH.InfixE (Just . toExp d $ a) (toExp d b) Nothing

toExp d (Expr.SectionR _ (unLoc -> a) (unLoc -> b))
  = TH.InfixE Nothing (toExp d a) (Just . toExp d $ b)

toExp _ (Expr.RecordCon _ name HsRecFields {rec_flds})
  = TH.RecConE (toName . unLoc $ name) (fmap toFieldExp rec_flds)

-- toExp (Expr.RecUpdate _ e xs)                 = TH.RecUpdE (toExp e) (fmap toFieldExp xs)
-- toExp (Expr.ListComp _ e ss)                  = TH.CompE $ map convert ss ++ [TH.NoBindS (toExp e)]
--  where
--   convert (Expr.QualStmt _ st)                = toStmt st
--   convert s                                   = noTH "toExp ListComp" s
-- toExp (Expr.ExpTypeSig _ e t)                 = TH.SigE (toExp e) (toType t)
--
#if MIN_VERSION_ghc(9, 2, 0)
toExp d (Expr.ExplicitList _ (map unLoc -> args)) = TH.ListE (map (toExp d) args)
#else
toExp d (Expr.ExplicitList _ _ (map unLoc -> args)) = TH.ListE (map (toExp d) args)
#endif

toExp d (Expr.ArithSeq _ _ e)
  = TH.ArithSeqE $ case e of
    (From a) -> TH.FromR (toExp d $ unLoc a)
    (FromThen a b) -> TH.FromThenR (toExp d $ unLoc a) (toExp d $ unLoc b)
    (FromTo a b) -> TH.FromToR (toExp d $ unLoc a) (toExp d $ unLoc b)
    (FromThenTo a b c) -> TH.FromThenToR (toExp d $ unLoc a) (toExp d $ unLoc b) (toExp d $ unLoc c)

#if MIN_VERSION_ghc(9, 6, 0)
toExp _ (Expr.HsProjection _ locatedFields) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = field_label <$> locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.ProjectionE (NonEmpty.map (unpackFS . unLoc . extractFieldLabel . unLoc) locatedFields)

toExp d (Expr.HsGetField _ expr locatedField) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = field_label <$> locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.GetFieldE (toExp d (unLoc expr)) (unpackFS . unLoc . extractFieldLabel . unLoc $ locatedField)
#elif MIN_VERSION_ghc(9, 4, 0)
toExp _ (Expr.HsProjection _ locatedFields) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.ProjectionE (NonEmpty.map (unpackFS . unLoc . extractFieldLabel . unLoc) locatedFields)

toExp d (Expr.HsGetField _ expr locatedField) =
  let
    extractFieldLabel (DotFieldOcc _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.GetFieldE (toExp d (unLoc expr)) (unpackFS . unLoc . extractFieldLabel . unLoc $ locatedField)
#elif MIN_VERSION_ghc(9, 2, 0)
toExp _ (Expr.HsProjection _ locatedFields) =
  let
    extractFieldLabel (HsFieldLabel _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.ProjectionE (NonEmpty.map (unpackFS . unLoc . extractFieldLabel . unLoc) locatedFields)

toExp d (Expr.HsGetField _ expr locatedField) =
  let
    extractFieldLabel (HsFieldLabel _ locatedStr) = locatedStr
    extractFieldLabel _ = error "Don't know how to handle XHsFieldLabel constructor..."
  in
    TH.GetFieldE (toExp d (unLoc expr)) (unpackFS . unLoc . extractFieldLabel . unLoc $ locatedField)
#endif


#if MIN_VERSION_ghc(9, 2, 0) && !MIN_VERSION_ghc(9, 6, 0)
toExp _ (Expr.HsOverLabel _ fastString) = TH.LabelE (unpackFS fastString)
#else
toExp _ (Expr.HsOverLabel _ _ fastString) = TH.LabelE (unpackFS fastString)
#endif

toExp dynFlags e = todo "toExp" (showSDoc dynFlags . ppr $ e)

todo :: (HasCallStack, Show e) => String -> e -> a
todo fun thing = error . concat $ [moduleName, ".", fun, ": not implemented: ", show thing]

noTH :: (HasCallStack, Show e) => String -> e -> a
noTH fun thing = error . concat $ [moduleName, ".", fun, ": no TemplateHaskell for: ", show thing]

moduleName :: String
moduleName = "Language.Haskell.Meta.Translate"
