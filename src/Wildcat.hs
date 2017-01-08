module Wildcat where

import Control.Monad
import Text.ParserCombinators.Parsec

data WildcatData =
  Args WildcatData |
  Atom String |
  List [WildcatData]
  deriving (Eq, Show)

withSurroundingSpaces :: Parser p -> Parser p
withSurroundingSpaces p = spaces >> p <* spaces

-- Base defs.
symbol :: Parser WildcatData
symbol = liftM Atom $ many1 letter

-- Compound defs.
commaSeparatedList :: Parser WildcatData -> Parser WildcatData
commaSeparatedList p = liftM List $ sepBy p $ try $ withSurroundingSpaces $ char ',' 

spaceSeparatedList :: Parser WildcatData -> Parser WildcatData
spaceSeparatedList p = liftM List $ many $ p <* spaces

spaceSeparatedArgs :: Parser WildcatData -> Parser WildcatData
spaceSeparatedArgs =
  liftM Args . between (char '(' >> spaces) (spaces >> char ')') . spaceSeparatedList
