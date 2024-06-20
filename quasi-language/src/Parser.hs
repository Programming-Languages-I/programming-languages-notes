module Parser where

import AST as AST
import Text.Parsec.String
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

parseStatement :: Parser Statement
parseStatement = parseAssigment

parseAssigment :: Parser Statement
parseAssigment = Assign <$> (parseIdentifier <* parseAssignSymbol) <*> (parseExpression)

parseAssignSymbol :: Parser Char
parseAssignSymbol = char ':' <* char '='

languageDef :: LanguageDef st
languageDef = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , identStart      = letter
  , identLetter     = alphaNum <|> oneOf "_'"
  , opStart         = opLetter emptyDef
  , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedNames   = ["BEGIN", "END"]
  , reservedOpNames = ["+", "-", "*", "/", ":=", "==", "<", ">", "<=", ">="]
  , caseSensitive   = True
  }

lexer :: TokenParser st
lexer = makeTokenParser languageDef

identifier = P.identifier lexer

parseIdentifier :: Parser String
parseIdentifier = many1 letter

parseExpression :: Parser Expression
-- parseExpression = parsePlus <|> parseMinus <|> parseTerm
parseExpression = parsePlus

-- parseExpression :: Parser (String, Int)
-- parseExpression = do
--     letters <- many1 letter
--     _ <- string ":="
--     d <- digit
--     return (letters, d)

parsePlus :: Parser Expression
parsePlus = AST.Plus <$> (parseTermFactor <* parsePlusSign) <*> parseTermFactor

parsePlusSign :: Parser Char
parsePlusSign = char '+'

parseTermFactor :: Parser Term
parseTermFactor = AST.TermFactor <$> parseFactor

parseFactor :: Parser Factor
parseFactor = AST.Value <$> parseDigit

parseDigit :: Parser Int
parseDigit = read <$> many1 digit
