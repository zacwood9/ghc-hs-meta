{-# LANGUAGE CPP #-}

module Main where

import Data.List.NonEmpty (nonEmpty)
import GHC.Meta.Parse
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

spec :: Spec
spec = do
#if MIN_VERSION_ghc(9,2,0)
  describe "Record dot syntax" $ do
    it "parses access" $ do
      let Right exp = parseExp "a.b"
      exp `shouldBe` TH.GetFieldE (TH.VarE (TH.mkName "a")) "b"
    it "parses single projection" $ do
      let Right exp = parseExp "(.b)"
      let Just list = nonEmpty ["b"]
      exp `shouldBe` TH.ProjectionE list
    it "parses multi projection" $ do
      let Right exp = parseExp "(.b.c.d)"
      let Just list = nonEmpty ["b", "c", "d"]
      exp `shouldBe` TH.ProjectionE list
  describe "Overloaded labels" $ do
    it "parses labels" $ do
      let Right exp = parseExp "#name"
      exp `shouldBe` TH.LabelE "name"
#endif
  describe "Type application" $ do
    it "parses application" $ do
      let Right exp = parseExp "a @b"
      exp `shouldBe` TH.AppTypeE (TH.VarE (TH.mkName "a")) (TH.VarT (TH.mkName "b"))

main = hspec spec