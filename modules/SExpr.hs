module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure []) -- Help I can't see the ground anymore

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (zeroOrMore p) -- What did I even just do

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> (zeroOrMore (satisfy isAlphaNum)) -- :O !

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- Returns a parser that allows whitespace on either side (and throws it away)
padded :: Parser a -> Parser a
padded p = spaces *> p <* spaces

-- Returns a parser that requires the string to be enclosed in parens (and throws them away)
inParens :: Parser a -> Parser a
inParens p = padded (char '(') *> p <* padded (char ')')

parseAtom :: Parser SExpr
parseAtom = A <$> (fmap N posInt <|> fmap I ident)

parseComb :: Parser SExpr
parseComb = Comb <$> (inParens $ oneOrMore (padded parseSExpr))

parseSExpr :: Parser SExpr
parseSExpr = padded (parseAtom <|> parseComb)
