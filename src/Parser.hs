module Parser where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Lexer
import Syntax
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String (Parser)

-- Operator table, lowest precedence last
operatorTable :: OperatorTable String () Identity Expr
operatorTable =
    [ [binary "^" Pow AssocRight],
      [binary "*" Mul AssocLeft],
        [ binary "+" Add AssocLeft,
          binary "-" Sub AssocLeft
        ],
        [ binary "<" Lt AssocNone,
          binary ">" Gt AssocNone,
          binary "<=" LtEq AssocNone,
          binary ">=" GtEq AssocNone,
          binary "==" Eq AssocNone,
          binary "!=" NotEq AssocNone
        ],
      [binaryWord "and" And AssocLeft],
      [binaryWord "or" Or AssocLeft]
    ]
  where
    binary op f = Infix (blueReservedOp op >> return (BinOp f))
    binaryWord w f = Infix (blueReserved w >> return (BinOp f))

boolExpr :: Parser Expr
boolExpr = Const . BoolV <$> ((True <$ blueReserved "true") <|> (False <$ blueReserved "false"))

quoteExpr :: Parser Expr
quoteExpr = do
  blueReserved "quote"
  Quote <$> blueParens expr

evalExpr :: Parser Expr
evalExpr = do
  blueReserved "eval"
  Eval <$> blueParens expr

expr :: Parser Expr
expr = buildExpressionParser operatorTable term

term :: Parser Expr
term =
  quoteExpr
    <|> evalExpr
    <|> blueParens expr
    <|> ifExpr
    <|> boolExpr
    <|> Const . NumV <$> blueInteger
    <|> Const . StrV <$> blueStringLiteral

ifExpr :: Parser Expr
ifExpr = do
    blueReserved "if"
    cond <- expr
    void blueColon
    thn <- expr
    els <- optionMaybe (blueReserved "else" *> blueColon *> expr)
    return (If cond thn els)

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (blueWhiteSpace *> expr <* eof) "<blue>"
