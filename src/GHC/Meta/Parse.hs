{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module is here to parse Haskell expression using the GHC Api
module GHC.Meta.Parse (parseExp, parseExpWithExts, parseExpWithFlags, parseHsExpr) where

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Parser.Errors.Ppr as ParserErrorPpr
import GHC.Driver.Config (initParserOpts)
import GHC.Parser.Annotation (LocatedA)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Parser.PostProcess
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Parser as Parser
import GHC.Data.FastString
import GHC.Types.SrcLoc
#else
import qualified SrcLoc
import DynFlags (DynFlags)
import Lexer (ParseResult (..), PState (..))
import StringBuffer
import Lexer
import qualified Parser
import FastString
import SrcLoc
import RdrName
import RdrHsSyn (runECP_P)
#endif

import GHC.Hs.Expr as Expr
import GHC.Hs.Extension as Ext
import qualified GHC.Meta.Settings as Settings
import qualified Language.Haskell.TH.Syntax as GhcTH
import qualified Language.Haskell.TH.Syntax as TH

import GHC.Meta.ToExp

-- | Parse a Haskell expression from source code into a Template Haskell expression.
-- See @parseExpWithExts@ or @parseExpWithFlags@ for customizing with additional extensions and settings.
parseExp :: String -> Either (Int, Int, String) TH.Exp
#if MIN_VERSION_ghc(9,2,0)
parseExp = parseExpWithExts 
    [ GhcTH.TypeApplications 
    , GhcTH.OverloadedRecordDot
    , GhcTH.OverloadedLabels
    , GhcTH.OverloadedRecordUpdate
    ]
#else
parseExp = parseExpWithExts 
    [ GhcTH.TypeApplications 
    ]
#endif

-- | Parse a Haskell expression from source code into a Template Haskell expression
-- using a given set of GHC extensions.
parseExpWithExts :: [GhcTH.Extension] -> String -> Either (Int, Int, String) TH.Exp
parseExpWithExts exts = parseExpWithFlags (Settings.baseDynFlags exts)

-- | Parse a Haskell expression from source code into a Template Haskell expression
-- using a given set of GHC DynFlags.
parseExpWithFlags :: DynFlags -> String -> Either (Int, Int, String) TH.Exp
parseExpWithFlags flags expStr = do
  hsExpr <- parseHsExpr flags expStr
  pure (toExp flags hsExpr)

-- | Run the GHC parser to parse a Haskell expression into a @HsExpr@.
parseHsExpr :: DynFlags -> String -> Either (Int, Int, String) (HsExpr GhcPs)
parseHsExpr dynFlags s =
  case runParser dynFlags s of
    POk _ locatedExpr ->
      let expr = SrcLoc.unLoc locatedExpr
       in Right
            expr

{- ORMOLU_DISABLE #-}
#if MIN_VERSION_ghc(9,2,0)
    -- TODO messages?
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, errors=errorMessages} ->
#elif MIN_VERSION_ghc(9,0,0)
    PFailed PState{loc=SrcLoc.psRealLoc -> srcLoc, messages=msgs} ->
#elif MIN_VERSION_ghc(8,10,0)
    PFailed PState{loc=srcLoc, messages=msgs} ->
#else
    -- TODO: check for pattern failure
    PFailed _ (SrcLoc.srcSpanEnd -> SrcLoc.RealSrcLoc srcLoc) doc ->
#endif
#if MIN_VERSION_ghc(9,2,0)
            let
                psErrToString e = show $ ParserErrorPpr.pprError e
                err = concatMap psErrToString errorMessages
                -- err = concatMap show errorMessages
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#elif MIN_VERSION_ghc(8,10,0)
            let -- TODO: do not ignore "warnMessages"
                -- I have no idea what they can be
                (_warnMessages, errorMessages) = msgs dynFlags
                err = concatMap show errorMessages
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#else
            let err = showSDoc dynFlags doc
                line = SrcLoc.srcLocLine srcLoc
                col = SrcLoc.srcLocCol srcLoc
            in Left (line, col, err)
#endif

-- From Language.Haskell.GhclibParserEx.GHC.Parser

parse :: P a -> String -> DynFlags -> ParseResult a
parse p str flags =
  Lexer.unP p parseState
  where
    location = mkRealSrcLoc (mkFastString "<string>") 1 1
    strBuffer = stringToStringBuffer str
    parseState =
#if MIN_VERSION_ghc(9, 2, 0)
      initParserState (initParserOpts flags) strBuffer location
#else
      mkPState flags strBuffer location
#endif

#if MIN_VERSION_ghc(9, 2, 0)
runParser :: DynFlags -> String -> ParseResult (LocatedA (HsExpr GhcPs))
runParser flags str =
  case parse Parser.parseExpression str flags of
    POk s e -> unP (runPV (unECP e)) s
    PFailed ps -> PFailed ps
#elif MIN_VERSION_ghc(8, 10, 0)
runParser :: DynFlags -> String -> ParseResult (Located (HsExpr GhcPs))
runParser flags str =
  case parse Parser.parseExpression str flags of
    POk s e -> unP (runECP_P e) s
    PFailed ps -> PFailed ps
#else
parseExpression = parse Parser.parseExpression
#endif
