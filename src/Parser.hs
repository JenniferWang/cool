module Parser where

import Prelude hiding (id)
import Text.Parsec
import Text.Parsec.String(Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

defaultType :: ParentType
defaultType = "object"

parseCool :: String -> Either ParseError [Class]
parseCool = parse program "<stdin>"

program :: Parser [Class]
program = Tok.whiteSpace lexer *> (many topClass) <* eof

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
  args <- parens (many formal)
  tp   <- (colon >> typeId)
  e    <- braces expr
  return $ Method (Formal name tp) args e

field :: Parser Feature
field = Field <$> formal <*> init
  where init =  try (Just <$> (reservedOp "<-" *> expr))
            <|> return Nothing

feature :: Parser Feature
feature = try method <|> try field <?> "Class method or field"

-- For those which incurs left-recursions, we'd better let this helper method
-- do the job
binary s = Ex.Infix (reservedOp s >> return (BinaryOp s)) Ex.AssocLeft
unary s = Ex.Prefix (reservedOp s >> return (UnaryOp s))

-- Table of operators and associativities
table = [ [ binary "."]
        , [ unary "~" ]
        , [ Ex.Prefix (reserved "isvoid" >> return IsVoid) ]
        , [ binary "*", binary "/" ]
        , [ binary "+", binary "-" ]
        , [ binary "<", binary "<=", binary "=" ]
        , [ unary "not" ]
        ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =  try function
      <|> try ifThenElse
      <|> try while
      <|> try block
      <|> try letIn
      <|> try caseOf
      <|> try new
      <|> try braced
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
dispatch :: Parser Expr
dispatch =  Dispatch
        <$> expr <*> maybeType <*> (dot *> identifier) <*> commaSep expr
  where maybeType =  try (Just <$> (reservedOp "@" *> typeId))
                 <|> return Nothing

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
block =  Block <$> braces ( many1 $ expr <* semi)

-- Type identifiers start with upper case
new :: Parser Expr
new = New <$> (reserved "new" >> typeId)

letIn :: Parser Expr
letIn = Let <$> (reserved "let" *> many1 clause)
            <*> (reserved "in"  *> expr)
  where clause =  (,) <$> formal <*> maybeExpr
        maybeExpr :: Parser (Maybe Expr)
        maybeExpr =  try (reservedOp "<-" *> (Just <$> expr))
                 <|> return Nothing

caseOf :: Parser Expr
caseOf = do
  e  <- (reserved "case") *> expr <* (reserved "of")
  cs <- (many1 clause) <* (reserved "esac")
  return $ Case e cs
  where clause =  (,) <$> formal <* reservedOp "=>"
                      <*> expr   <* semi

braced :: Parser Expr
braced = Braced <$> braces expr

int :: Parser Expr
int = Int <$> integer

-- stringLiteral will take care of the escape
string :: Parser Expr
string = String <$> stringLiteral

boolean :: Parser Expr
boolean = Boolean <$> (try true <|> try false <?> "Boolean")
  where true  = reserved "true"  *> return True
        false = reserved "false" *> return False




