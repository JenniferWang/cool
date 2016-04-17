{-# LANGUAGE FlexibleContexts #-}
module Lexer where

import Text.Parsec.String(Parser)
import Text.Parsec.Prim((<|>), try, many)
import Data.Char(toLower, toUpper)
import Control.Monad(msum)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Char as C

-- TODO: In Cool manual, the keyword is case insensitive; however, it is clumsy to
-- deal with it in general with a parser combinator. We could either use a parser
-- generator or scan and replace all the keyword with lower case.
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops     = [ "+", "*", "-", "/", "~", "<", "<=", "=", "<-"
                  , ".", "@" ]
        keyword = [ "class", "else", "fi", "if", "in", "inherits", "isvoid"
                  , "let", "loop", "pool", "then", "while", "case", "esac"
                  , "new", "of", "not", "true", "false"
                  ]
        style = Tok.LanguageDef
                  { Tok.commentStart    = "(*"
                  , Tok.commentEnd      = "*)"
                  , Tok.commentLine     = "--"
                  , Tok.nestedComments  = True
                  , Tok.identStart      = C.lower
                  , Tok.identLetter     = C.alphaNum <|> C.char '_'
                  , Tok.opStart         = Tok.opLetter style
                  , Tok.opLetter        = C.oneOf "+-*/<=>-~@.;"  -- TODO
                  , Tok.reservedNames   = keyword
                  , Tok.reservedOpNames = ops
                  , Tok.caseSensitive   = True
                  }

integer :: Parser Integer
integer = Tok.integer lexer

typeId :: Parser String
typeId = Tok.lexeme lexer id
  where id = (:) <$> C.upper <*> many (C.alphaNum <|> C.char '_')

identifier = Tok.identifier lexer

stringLiteral = Tok.stringLiteral lexer

parens = Tok.parens lexer

braces = Tok.braces lexer

colon = Tok.colon lexer

dot = Tok.dot lexer

comma = Tok.comma lexer

semi = Tok.semi lexer

semiSep = Tok.semiSep lexer

commaSep = Tok.commaSep lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

