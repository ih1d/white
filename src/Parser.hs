-- |
-- Module      :  Parser
-- Copyright   :  (c) Isaac Hiram Lopez Diaz 2026
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  isaac.lopez@upr.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser for Blue+ surface syntax. Produces the AST from "Syntax".
module Parser (
    parseExpr,
    parseType,
    expression,
    typeExpr,
) where

import Control.Monad.Combinators.Expr
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Lexer
import Syntax
import Text.Megaparsec

-----------------------------------------------------------
---- Entry points
-----------------------------------------------------------

-- | Parse a complete Blue+ expression from input text.
parseExpr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> expression <* eof)

-- | Parse a Blue+ type.
parseType :: FilePath -> Text -> Either (ParseErrorBundle Text Void) Type
parseType = runParser (sc *> typeExpr <* eof)

-----------------------------------------------------------
---- Types
-----------------------------------------------------------

-- | A type expression: arrows are right-associative, products bind tighter
-- than arrows, and @Code@ binds tightest (over a single atom).
typeExpr :: Parser Type
typeExpr = do
    a <- typeProd
    option a $ do
        eff <- arrowAnnot
        b <- typeExpr
        pure (ArrowT a eff b)

typeProd :: Parser Type
typeProd = makeExprParser typeAtom [[InfixL (ProdT <$ op "*")]]

typeAtom :: Parser Type
typeAtom =
    choice
        [ IntT <$ reserved "Int"
        , BoolT <$ reserved "Bool"
        , StringT <$ reserved "String"
        , CodeT <$> (reserved "Code" *> typeAtom)
        , parens typeExpr
        ]

-- | Either a pure arrow @->@ or an effectful arrow @-{l1, ..., ln}->@.
arrowAnnot :: Parser Effect
arrowAnnot = try effRow <|> (Pure <$ op "->")
  where
    effRow = do
        _ <- symbol "-"
        labels <- braces (effLabel `sepBy` comma)
        op "->"
        pure (Eff (Set.fromList labels))

effLabel :: Parser EffLabel
effLabel =
    choice
        [ Reflect <$ reserved "Reflect"
        , IO <$ reserved "IO"
        , Diverge <$ reserved "Diverge"
        ]

-----------------------------------------------------------
---- Expressions
-----------------------------------------------------------

-- | A complete Blue+ expression.
expression :: Parser Expr
expression =
    choice
        [ lambdaExpr
        , letExpr
        , ifExpr
        , binExpr
        ]

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- symbol "\\"
    (x, t) <- parens $ do
        x <- identifier
        _ <- op ":"
        t <- typeExpr
        pure (x, t)
    _ <- op "->"
    LamE x t <$> expression

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    x <- identifier
    _ <- op "="
    rhs <- expression
    reserved "in"
    LetE x rhs <$> expression

ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    c <- expression
    reserved "then"
    t <- expression
    reserved "else"
    IfE c t <$> expression

-- | Binary / unary operators, applied on top of application.
binExpr :: Parser Expr
binExpr = makeExprParser appExpr operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [binary "^" Pow]
    , [prefixSym "-" Neg]
    , [binary "*" Mul]
    , [binary "+" Add, binary "-" Sub]
    ,
        [ binaryN "==" Syntax.Eq
        , binaryN "!=" NotEq
        , binaryN "<=" LtEq
        , binaryN ">=" GtEq
        , binaryN "<" Lt
        , binaryN ">" Gt
        ]
    , [prefixWord "not" Not]
    , [binaryWord "and" And]
    , [binaryWord "or" Or]
    ]
  where
    binary s o = InfixL (BinE o <$ op s)
    binaryN s o = InfixN (BinE o <$ op s)
    binaryWord w o = InfixL (BinE o <$ reserved w)
    prefixSym s o = Prefix (UnE o <$ op s)
    prefixWord w o = Prefix (UnE o <$ reserved w)

-- | @em@ is a prefix on application-level expressions; otherwise
-- juxtaposition is left-associative function application.
appExpr :: Parser Expr
appExpr =
    (EmE <$> (reserved "em" *> appExpr))
        <|> (foldl1 AppE <$> some postfixExpr)

-- | Atom optionally suffixed by @.fst@ / @.snd@ projections.
postfixExpr :: Parser Expr
postfixExpr = do
    e <- antiOrAtom
    suffixes e
  where
    antiOrAtom = antiExpr <|> atomExpr
    suffixes e = (proj e >>= suffixes) <|> pure e
    proj e =
        choice
            [ FstE e <$ try (dot *> reserved "fst")
            , SndE e <$ try (dot *> reserved "snd")
            ]

-- | Antiquotation @~ atom@. Binds tighter than @.fst@ / @.snd@, so
-- @~v.fst@ parses as @(~v).fst@.
antiExpr :: Parser Expr
antiExpr = AntiE <$> (op "~" *> atomExpr)

atomExpr :: Parser Expr
atomExpr =
    choice
        [ ConstE <$> literal
        , quoteExpr
        , parenOrPair
        , VarE <$> identifier
        ]

literal :: Parser Value
literal =
    choice
        [ IntV <$> integer
        , BoolV True <$ reserved "True"
        , BoolV False <$ reserved "False"
        , StrV <$> stringLit
        ]

quoteExpr :: Parser Expr
quoteExpr = QuoteE <$> between quoteOpen quoteClose expression

-- | A parenthesized expression, or a 2-tuple literal.
parenOrPair :: Parser Expr
parenOrPair = parens $ do
    e1 <- expression
    option e1 (PairE e1 <$> (comma *> expression))
