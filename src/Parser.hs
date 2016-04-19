module Parser where

import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.String(Parser)
import Text.Parsec.Combinator

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

defaultType :: ParentType
defaultType = "object"

parseCool :: String -> Either ParseError [Class]
parseCool = parse program "<stdin>"

program :: Parser [Class]
program = Tok.whiteSpace lexer *> (endBy topClass semi) <* eof

topClass :: Parser Class
topClass = Class <$> (reserved "class" *> typeId)
                 <*> inherit
                 <*> braces (many $ feature <* semi)
           where inherit =  try (reserved "inherits" *> typeId)
                        <|> return defaultType

formal :: Parser Formal
formal = Formal <$> identifier <* colon
                <*> typeId

method :: Parser Feature
method = do
  name <- identifier
  args <- parens (commaSep formal)
  tp   <- (colon >> typeId)
  e    <- braces expr
  return $ Method (Formal name tp) args e

field :: Parser Feature
field = Field <$> formal <*> init
  where init =  optionMaybe (reservedOp "<-" *> expr)

feature :: Parser Feature
feature = try method <|> try field <?> "Class method or field"

binary s op = Ex.Infix (reservedOp s >> return (BinaryOp op)) Ex.AssocLeft
unary s op = Ex.Prefix (reservedOp s >> return (UnaryOp op))

-- Table of operators and associativities
table = [ [ Ex.Infix dispatch Ex.AssocLeft                 ]
        , [ unary "~" Negate                               ]
        , [ Ex.Prefix (reserved "isvoid" >> return IsVoid) ]
        , [ binary "*" Multiply, binary "/" Divide         ]
        , [ binary "+" Plus    , binary "-" Minus          ]
        , [ binary "<" LessThan, binary "<=" LessEqual
          , binary "=" Equal                               ]
        , [ unary "not" Not                                ]
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =  try assign
      <|> try function
      <|> try ifThenElse
      <|> try while
      <|> try block
      <|> try letIn
      <|> try caseOf
      <|> try new
      <|> try (parens expr)
      <|> try id              -- terminals
      <|> try int
      <|> try boolean
      <|> try Parser.string

-- as "<-" has least precedence and right associative, we could save the work to
-- include it in the table
assign :: Parser Expr
assign = Assign <$> identifier
                <*> (reservedOp "<-" *> expr)

-- dispatch operator "." is left associative
-- like "x.foo().bar() = (x.foo()).bar()"
dispatch :: Parser (Expr -> Expr -> Expr)
dispatch = try static <|> try regular
  where
  static = StaticDispatch <$> (reservedOp "@" *> typeId <* reservedOp ".")
  regular = do { reservedOp "."; return Dispatch }

function :: Parser Expr
function =  Function <$> identifier <*> parens (commaSep expr)

ifThenElse :: Parser Expr
ifThenElse =  IfThenElse
          <$> (reserved "if"   *> expr)
          <*> (reserved "then" *> expr)
          <*> (reserved "else" *> expr <* reserved "fi")

while :: Parser Expr
while = While <$> (reserved "while" *> expr <* reserved "loop")
              <*> (expr <* reserved "pool")

-- None-Type identifiers start with lower case
id :: Parser Expr
id = ID <$> identifier

block :: Parser Expr
block =  Block <$> braces (endBy1 expr semi)

-- Type identifiers start with upper case
new :: Parser Expr
new = New <$> (reserved "new" >> typeId)

letIn :: Parser Expr
letIn = Let <$> (reserved "let" *> many1 clause)
            <*> (reserved "in"  *> expr)
  where clause =  (,) <$> formal <*> maybeExpr
        maybeExpr :: Parser (Maybe Expr)
        maybeExpr =  optionMaybe (reservedOp "<-" *> expr)

caseOf :: Parser Expr
caseOf = do
  e  <- (reserved "case") *> expr <* (reserved "of")
  cs <- (many1 clause) <* (reserved "esac")
  return $ Case e cs
  where clause =  (,) <$> formal <* reservedOp "=>"
                      <*> expr   <* semi

int :: Parser Expr
int = Int <$> integer

-- stringLiteral will take care of the escape
string :: Parser Expr
string = String <$> stringLiteral

boolean :: Parser Expr
boolean = Boolean <$> (try true <|> try false <?> "Boolean")
  where true  = reserved "true"  *> return True
        false = reserved "false" *> return False




