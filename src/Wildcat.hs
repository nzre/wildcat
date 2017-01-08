module Wildcat where

import Control.Monad
import Text.ParserCombinators.Parsec

data WildcatData =
  Atom String
  deriving (Eq, Show)

symbol :: Parser WildcatData
symbol = liftM Atom $ many1 letter
