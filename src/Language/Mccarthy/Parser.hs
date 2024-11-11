{-# LANGUAGE OverloadedStrings #-}

module Language.Mccarthy.Parser (readExpr) where

import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (
  char,
  letter,
  oneOf,
  digit,
  (<|>),
  try,
  parse,
  many,
  sepBy,
  spaces,
  between,
  eof)
import Data.Functor.Identity (Identity)
import Language.Mccarthy.Types (LispVal (..), Eval, LispException (..))
import Control.Monad.Except (throwError)

lexer :: Tok.GenTokenParser Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef Text () Identity
style = Lang.emptyDef {
  Tok.commentStart = "#|"
  , Tok.commentEnd = "|#"
  , Tok.commentLine = ";"
  , Tok.opStart = Tok.opLetter style
  , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  , Tok.reservedOpNames = [ "'", "\""]
  }

Tok.TokenParser { Tok.parens = m_parens
                , Tok.identifier = m_identifier } = Tok.makeTokenParser style

reservedOp :: Text -> Parser ()
reservedOp op = Tok.reservedOp lexer $ Text.unpack op

parseAtom :: Parser LispVal
parseAtom = do { p <- m_identifier; return $ Atom $ Text.pack p }

parseList :: Parser LispVal
parseList = List . concat <$> m_parens (many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseDottedList :: Parser LispVal
parseDottedList = m_parens $ do
  lhead <- many parseExpr `sepBy` (char ' ' <|> char '\n')
  _ <- between spaces spaces $ char '.'
  ltail <- parseExpr
  return $ DottedList (head lhead) ltail

parseQuote :: Parser LispVal
parseQuote = do { reservedOp "\'"; expr <- parseExpr; return $ List [Atom "quote", expr] }

parseT :: Parser LispVal
parseT = reservedOp "t" >> return T

parseNil :: Parser LispVal
parseNil = reservedOp "()" >> return Nil

parseExpr :: Parser LispVal
parseExpr = parseT <|> parseNil <|> parseQuote <|> parseAtom <|> try parseDottedList <|> parseList

contents :: Parser a -> Parser a
contents p = do { Tok.whiteSpace lexer; r <- p; eof; return r }

readExpr :: Text -> Eval LispVal
readExpr input = case parse (contents parseExpr) "<stdin>" input of
  Left err -> throwError . PError . show $ err
  Right expr -> return expr
