module WildcatTest where

import qualified Wildcat as W

import Test.Hspec
import Text.ParserCombinators.Parsec

atom = W.Atom

symbol = parse W.symbol "symbol"

main :: IO ()
main = hspec $ do
  describe "symbol" $ do
    it "correctly parses a single a-z character as a symbol" $ do
      symbol "a" `shouldBe` (Right $ atom "a")
      symbol "z" `shouldBe` (Right $ atom "z")
    it "correctly parses multiple a-z characters as a symbol" $ do
      symbol "abc" `shouldBe` (Right $ atom "abc")
      symbol "xyz" `shouldBe` (Right $ atom "xyz")
