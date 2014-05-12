{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------------------------
-- Read.hs - Read S-Expression
----------------------------------------------------------------
module Read (
  readExp) where       
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import Type

readExp :: MonadError ScmError m => String -> m ScmExp
readExp input = case parse parseExp "lisp" input of
  Left err -> throwError $ ScmParserError err
  Right x -> return x

-- utility
symbol :: Parser Char
symbol = oneOf "!#$%*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

spaces0 :: Parser ()
spaces0 = skipMany space
-- parser

parseString :: Parser ScmExp
parseString = do
  spaces0
  char '"'
  x <- many (noneOf "\"")
  char '"'
  spaces0
  return $ ScmString x

parseSymbol :: Parser ScmExp
parseSymbol = do
  spaces0
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let name = first:rest
  return $ case name of
    "#t" -> ScmBool True
    "#f" -> ScmBool False
    _ -> ScmSymbol name
    
-- TODO; parse negative number
parseNumber :: Parser ScmExp
parseNumber = do
  spaces0
  n <- many1 digit
  return $ ScmInt (read n)

parseList :: Parser ScmExp
parseList = do
  elts <- many parseExp
  return $ foldr ScmCons ScmEmptyList elts

--
parseDotted :: Parser ScmExp
parseDotted = do
  elts <- many1 parseExp
  char '.'
  spaces
  tailPart <- parseExp
  return $ foldr ScmCons tailPart elts

parseQuoted :: Parser ScmExp
parseQuoted = do
  char '\''
  x <- parseExp
  return $ ScmCons (ScmSymbol "quote") (ScmCons x ScmEmptyList)

parseExp :: Parser ScmExp
parseExp = do
  spaces0
  res <- try parseSymbol
         <|> try parseString
         <|> try parseNumber
         <|> try parseQuoted
         <|> do 
           spaces0
           char '('
           x <- try parseDotted <|> parseList
           spaces0
           char ')'
           return x
  spaces0
  return res
