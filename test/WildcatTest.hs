module WildcatTest where

import qualified Wildcat as W

import Test.Hspec
import Text.ParserCombinators.Parsec

args = W.Args
atom = W.Atom
list = W.List

atomList = list . map atom

symbol = parse W.symbol "symbol"

commaSeparatedArgs p = parse (W.commaSeparatedArgs p) "commaSeparatedArgs"
commaSeparatedList p = parse (W.commaSeparatedList p) "commaSeparatedList"
spaceSeparatedArgs p = parse (W.spaceSeparatedArgs p) "spaceSeparatedArgs"
spaceSeparatedList p = parse (W.spaceSeparatedList p) "spaceSeparatedList"

main :: IO ()
main = hspec $ do
  describe "symbol" $ do
    it "correctly parses a single a-z character as a symbol" $ do
      symbol "a" `shouldBe` (Right $ atom "a")
      symbol "z" `shouldBe` (Right $ atom "z")
    it "correctly parses multiple a-z characters as a symbol" $ do
      symbol "abc" `shouldBe` (Right $ atom "abc")
      symbol "xyz" `shouldBe` (Right $ atom "xyz")
  describe "commaSeparatedList" $ do
    it "correctly handles an empty list" $ do
      spaceSeparatedList W.symbol "" `shouldBe` (Right $ list [])
    it "correctly handles spaces preceding and trailing commas" $ do
      commaSeparatedList W.symbol "a,b" `shouldBe` (Right $ atomList ["a", "b"])
      commaSeparatedList W.symbol "a, b" `shouldBe` (Right $ atomList ["a", "b"])
      commaSeparatedList W.symbol "a ,b" `shouldBe` (Right $ atomList ["a", "b"])
      commaSeparatedList W.symbol "a , b" `shouldBe` (Right $ atomList ["a", "b"])
    it "ignores trailing whitespace" $ do
      commaSeparatedList W.symbol "a, b " `shouldBe` (Right $ atomList ["a", "b"])
  describe "spaceSeparatedList" $ do
    it "correctly handles an empty list" $ do
      spaceSeparatedList W.symbol "" `shouldBe` (Right $ list [])
    it "correctly parses a space-separated list" $ do
      spaceSeparatedList W.symbol "a b" `shouldBe` (Right $ atomList ["a", "b"])
      spaceSeparatedList W.symbol "a  b" `shouldBe` (Right $ atomList ["a", "b"])
    it "ignores trailing whitespace" $ do
      spaceSeparatedList W.symbol "a b " `shouldBe` (Right $ atomList ["a", "b"])
  describe "commaSeparatedArgs" $ do
    it "correctly handles an empty arg list" $ do
      commaSeparatedArgs W.symbol "()" `shouldBe` (Right $ args $ list [])
      commaSeparatedArgs W.symbol "( )" `shouldBe` (Right $ args $ list [])
    it "correctly parses a comma-separated arg list" $ do
      commaSeparatedArgs W.symbol "(a)" `shouldBe` (Right $ args $ atomList ["a"])
      commaSeparatedArgs W.symbol "(a, b)" `shouldBe` (Right $ args $ atomList ["a", "b"])
      commaSeparatedArgs W.symbol "(a , b)" `shouldBe` (Right $ args $ atomList ["a", "b"])
    it "ignores whitespace near parentheses" $ do
      commaSeparatedArgs W.symbol "( a )" `shouldBe` (Right $ args $ atomList ["a"])
    it "ignores whitespace after ')'" $ do
      commaSeparatedArgs W.symbol "(a) " `shouldBe` (Right $ args $ atomList ["a"])
  describe "spaceSeparatedArgs" $ do
    it "correctly handles an empty arg list" $ do
      spaceSeparatedArgs W.symbol "()" `shouldBe` (Right $ args $ list [])
      spaceSeparatedArgs W.symbol "( )" `shouldBe` (Right $ args $ list [])
    it "correctly parses a space-separated arg list" $ do
      spaceSeparatedArgs W.symbol "(a)" `shouldBe` (Right $ args $ atomList ["a"])
      spaceSeparatedArgs W.symbol "(a b)" `shouldBe` (Right $ args $ atomList ["a", "b"])
      spaceSeparatedArgs W.symbol "(a  b)" `shouldBe` (Right $ args $ atomList ["a", "b"])
    it "ignores whitespace near parentheses" $ do
      spaceSeparatedArgs W.symbol "( a )" `shouldBe` (Right $ args $ atomList ["a"])
    it "ignores whitespace after ')'" $ do
      spaceSeparatedArgs W.symbol "(a) " `shouldBe` (Right $ args $ atomList ["a"])
