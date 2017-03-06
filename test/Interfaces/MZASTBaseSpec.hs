module Interfaces.MZASTBaseSpec (main, spec) where

import Interfaces.MZASTBase

import Test.Hspec
import Test.QuickCheck

import Generic.Random.Generic

instance Arbitrary GArguments where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Expr where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Annotation where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Op where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Item where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Solve where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Declaration where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary DeclarationSignature where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Type where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary Inst where
  arbitrary = genericArbitrary' Z uniform

instance Arbitrary AnnExpr where
  arbitrary = genericArbitrary' Z uniform
  

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stripExprOff" $ do
    it "should strip the annotations from an expression" $ property $
      \e ans -> stripExprOff (AnnExpr e ans) `shouldBe` e
