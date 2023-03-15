{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-fields -Wno-name-shadowing -Wno-unused-imports #-}

-- | Settings needed for running the GHC Parser.
module Language.Haskell.Meta.Settings (baseDynFlags) where

{- ORMOLU_DISABLE -}

#if MIN_VERSION_ghc(9,0,0)
import GHC.Settings.Config
import GHC.Driver.Session
import GHC.Utils.Fingerprint
import GHC.Platform
import GHC.Settings
#elif MIN_VERSION_ghc(8, 10, 0)
import Config
import DynFlags
import Fingerprint
import GHC.Platform
import ToolSettings
#else
import Config
import DynFlags
import Fingerprint
import Platform
#endif

#if MIN_VERSION_ghc(8,10,0)
import GHC.Hs
#else
import HsSyn
#endif

#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Driver.Config
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Parser.PostProcess
import GHC.Driver.Session
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Parser as Parser
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Driver.Backpack.Syntax
import GHC.Unit.Info
import GHC.Types.Name.Reader
#else
import StringBuffer
import Lexer
import qualified Parser
import FastString
import SrcLoc
import RdrName
#endif

#if MIN_VERSION_ghc(9, 0, 0)
#else
import RdrHsSyn
#endif

import Data.Data hiding (Fixity)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Hs

#if MIN_VERSION_ghc(9, 2, 0)
import GHC.Types.Fixity
import GHC.Types.SourceText
#else
import GHC.Types.Basic
#endif

import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.SrcLoc
#elif MIN_VERSION_ghc(8, 10, 0)
import BasicTypes
import OccName
#else
import BasicTypes
import OccName
#endif

import qualified Language.Haskell.TH.Syntax as GhcTH
import Data.Maybe

fakeSettings :: Settings
fakeSettings = Settings
#if MIN_VERSION_ghc(9, 2, 0)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
#elif MIN_VERSION_ghc(8, 10, 0)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
  , sToolSettings=toolSettings
  }
#else
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
#endif
  where
#if MIN_VERSION_ghc(8, 10, 0)
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
#endif
    platform =
      Platform{
#if MIN_VERSION_ghc(9, 0, 0)
    -- It doesn't matter what values we write here as these fields are
    -- not referenced for our purposes. However the fields are strict
    -- so we must say something.
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
#if MIN_VERSION_ghc(9, 2, 0)
      , platform_constants=platformConstants
#endif
      ,
#endif

#if MIN_VERSION_ghc(9, 2, 0)
        platformWordSize=PW8
      , platformArchOS=ArchOS {archOS_arch=ArchUnknown, archOS_OS=OSUnknown}
#elif MIN_VERSION_ghc(8, 10, 0)
        platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#else
        platformWordSize=8
      , platformOS=OSUnknown
#endif

#if MIN_VERSION_ghc(9, 4, 0)
      , platformHasLibm = False
#endif
      , platformUnregisterised=True
      }
#if MIN_VERSION_ghc(9, 2, 0)
    platformConstants = Nothing
#else
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}
#endif

#if MIN_VERSION_ghc(9, 6, 0)
applyFakeLlvmConfig :: a -> a
applyFakeLlvmConfig = id
#elif MIN_VERSION_ghc(8, 10, 0)
applyFakeLlvmConfig :: (LlvmConfig -> a) -> a
applyFakeLlvmConfig f = f $ LlvmConfig [] []
#else
applyFakeLlvmConfig :: (LlvmTargets, LlvmPasses)
applyFakeLlvmConfig f = f ([], [])
#endif

baseDynFlags :: [GhcTH.Extension] -> DynFlags
baseDynFlags exts =
  let enable = GhcTH.TemplateHaskellQuotes : exts
   in foldl xopt_set (applyFakeLlvmConfig $ defaultDynFlags fakeSettings) enable
