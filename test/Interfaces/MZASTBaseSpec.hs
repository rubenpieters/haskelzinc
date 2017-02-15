module Interfaces.MZASTBaseSpec (main, spec) where

import Interfaces.MZASTBase

import Test.Hspec
import Test.QuickCheck

import Data.DeriveTH

derive makeArbitrary ''Expr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "stripExprOff" $ do
    it "should strip the annotations from an expression" $ property $
      \e ans -> stripExprOff (AnnExpr e ans) `shouldBe` e