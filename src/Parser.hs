module Parser where

import           Data.Char
import           Control.Monad
import           Control.Applicative

{-
    A Parser is a function that takes an input stream of characters
    and outputs a parse tree by applying the parser login over lexemes
    to build up a composite data structure for the AST
-}
newtype Parser a = Parser {
    parse :: String -> [(a, String)]
}
