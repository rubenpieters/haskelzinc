module Interfaces.MZASTBaseSpec (main, spec) where

import Interfaces.MZASTBase

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stripExprOff" $ do
    it "should only strip off expressions" $ do
      stripExprOff ( AnnExpr AnonVar [Annotation "testAnn" []] ) `shouldBe` AnonVar
