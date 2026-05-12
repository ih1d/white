-- |
-- Module      :  Lexer
-- Copyright   :  (c) Isaac Hiram Lopez Diaz 2026
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  isaac.lopez@upr.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- Tokenizer for the Blue+ language. Built on Megaparsec.
module Lexer (
    Parser,
    sc,
    lexeme,
    symbol,
    parens,
    brackets,
    braces,
    quoteOpen,
    quoteClose,
    reserved,
    reservedWords,
    identifier,
    integer,
    stringLit,
    op,
    dot,
    comma,
) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

-- | The concrete parser monad: parses 'Text', has no custom error component
-- or user state.
type Parser = Parsec Void Text

-- | Whitespace and comment consumer.
sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "#") (L.skipBlockCommentNested "#|" "|#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens, brackets, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
braces = between (symbol "{") (symbol "}")

-- | Open / close of a quotation: @<| ... |>@.
quoteOpen, quoteClose :: Parser ()
quoteOpen = void (symbol "<|")
quoteClose = void (symbol "|>")

dot, comma :: Parser ()
dot = void (symbol ".")
comma = void (symbol ",")

-- | Characters that can continue a multi-character operator. Single-char
-- operators use 'notFollowedBy' against this set so that, e.g., parsing
-- @<@ does not accidentally succeed when the input is @<=@ or @<|@.
opContChars :: [Char]
opContChars = ":!<>=+-*^/|{"

-- | Match a literal operator, ensuring it isn't a prefix of a longer one.
op :: Text -> Parser ()
op s = void $ (lexeme . try) (C.string s <* notFollowedBy (oneOf opContChars))

-- | Words that may not be used as identifiers.
reservedWords :: [Text]
reservedWords =
    [ "let"
    , "in"
    , "if"
    , "then"
    , "else"
    , "True"
    , "False"
    , "and"
    , "or"
    , "not"
    , "em"
    , "fst"
    , "snd"
    , "Int"
    , "Bool"
    , "String"
    , "Code"
    , "Reflect"
    , "IO"
    , "Diverge"
    ]

-- | Match a reserved word exactly (not followed by an identifier char).
reserved :: Text -> Parser ()
reserved w =
    (lexeme . try) (C.string w *> notFollowedBy (C.alphaNumChar <|> C.char '_'))

-- | Identifier: a letter-or-underscore start, followed by alphanumerics or
-- underscores, that is not a reserved word.
identifier :: Parser Text
identifier = (lexeme . try) (raw >>= check)
  where
    raw =
        T.pack
            <$> ( (:)
                    <$> (C.letterChar <|> C.char '_')
                    <*> many (C.alphaNumChar <|> C.char '_')
                )
    check x
        | x `elem` reservedWords =
            fail $ "keyword " <> show x <> " cannot be an identifier"
        | otherwise = pure x

integer :: Parser Integer
integer = lexeme L.decimal

-- | Double-quoted string literal with standard Haskell escapes.
stringLit :: Parser Text
stringLit =
    T.pack
        <$> (lexeme . try) (C.char '"' *> manyTill L.charLiteral (C.char '"'))
