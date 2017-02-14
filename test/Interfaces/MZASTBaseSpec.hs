module Interfaces.MZASTBaseSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "temporary sanity check test" $ do
    it "should pass" $ do
      "a" `shouldBe` "a"