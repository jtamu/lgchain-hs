module MyLibSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "trivial" $ do
  it "True shoud be True" $ do
    True `shouldBe` True
