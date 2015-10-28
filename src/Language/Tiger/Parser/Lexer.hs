{-# LANGUAGE OverloadedStrings #-}


module Language.Tiger.Lexer
       ( Token(..)
       , PToken
       , TokenParser
       , Tokenizer
       , tokenizer
       , tokenize
       , eat
       , getString
       , getInt ) where

import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))
import Data.Text (Text)

data Token = Ident  String
           | StrLit String
           | IntLit Int     -- Int would be sufficient because we don't support infinite precision integer in Tiger.
           | Plus
           | Minus
           | Mult
           | Div
           | Eq
           | Ge
           | Gt
           | Le
           | Lt
           | Neq
           | Dot
           | Comma
           | And
           | Or
           | Colon
           | Semi
           | Assign
           | LBrace
           | RBrace
           | LParan
           | RParan
           | LBrack
           | RBrack
           | Var
           | Function
           | Break
           | Of
           | End
           | In
           | Nil
           | Let
           | Do
           | To
           | For
           | While
           | Else
           | Then
           | If
           | Array
           | Type
           deriving (Show, Eq)

type PToken = (Token, P.SourcePos)
type Symbol = String

type TextParsec = P.Parsec Text ()
type Tokenizer = TextParsec [PToken]
type SingleToken = TextParsec PToken

type TokenParser a = P.Parsec [PToken] () a

tokenizer :: Tokenizer
tokenizer = skip *> P.many (singleToken <* skip) <* P.eof

singleToken :: SingleToken
singleToken = P.choice [
    P.try $ "<>" `symbolToken` Neq
  , P.try $ "<=" `symbolToken` Le
  , P.try $ ">=" `symbolToken` Ge
  , P.try $ ":=" `symbolToken` Assign
  , '<'          `charToken` Lt
  , '>'          `charToken` Gt
  , '='          `charToken` Eq
  , '+'          `charToken` Plus
  , '-'          `charToken` Minus
  , '*'          `charToken` Mult
  , '/'          `charToken` Div
  , '('          `charToken` LParan
  , ')'          `charToken` RParan
  , '['          `charToken` LBrack
  , ']'          `charToken` RBrack
  , '{'          `charToken` LBrace
  , '}'          `charToken` RBrace
  , '.'          `charToken` Dot
  , ';'          `charToken` Semi
  , ':'          `charToken` Colon
  , '|'          `charToken` Or
  , '&'          `charToken` And
  , "var"        `keyword` Var
  , "function"   `keyword` Function
  , "break"      `keyword` Break
  , "of"         `keyword` Of
  , "end"        `keyword` End
  , "in"         `keyword` In
  , "nil"        `keyword` Nil
  , "let"        `keyword` Let
  , "do"         `keyword` Do
  , "to"         `keyword` To
  , "for"        `keyword` For
  , "while"      `keyword` While
  , "else"       `keyword` Else
  , "then"       `keyword` Then
  , "if"         `keyword` If
  , "array"      `keyword` Array
  , "type"       `keyword` Type
  , identifier
  , stringLit
  , intLit
  ]

skip :: TextParsec ()
skip = P.skipMany $ P.string "/*" *> P.manyTill P.anyChar (P.try $ P.string "*/")
       <|> P.many1 P.space

pTokenPos :: PToken -> P.SourcePos
pTokenPos = snd

charToken :: Char -> Token -> SingleToken
charToken ch tok = pTok tok <* P.char ch <?> [ch]

symbolToken :: Symbol -> Token -> SingleToken
symbolToken sym tok = pTok tok <* P.string sym <?> sym

keyword :: Symbol -> Token -> SingleToken
keyword key tok = P.try
                  (pTok tok <* P.string key <* P.notFollowedBy P.alphaNum) <?> key

pTok :: Token -> SingleToken
pTok tok = (,) <$> pure tok <*> P.getPosition

identifier :: SingleToken
identifier = do
  { pos <- P.getPosition
  ; first <- P.letter <|> P.char '_'
  ; rest <- P.many $ P.alphaNum <|> P.char '_'
  ; return (Ident (first:rest), pos) } <?> "Identifier"

stringLit :: SingleToken
stringLit = do
  { pos <- P.getPosition
  ; lit <- literal
  ; return (StrLit lit, pos) } <?> "String Literal"
  where literal = P.between (P.char '\"') (P.char '\"') $ P.many singleChar
        singleChar = P.noneOf "\"\\" <|> escaped
        escaped = P.char '\\' *> P.choice (zipWith (\c r -> P.char c *> pure r) code real)
        code = ['b', 'f', 'n', 'r', 't', '\\', '\"', '/']
        real = ['\b', '\f', '\n', '\r', '\t', '\\', '\"', '/']

intLit :: SingleToken
intLit = do
  { pos <- P.getPosition
  ; dec <- P.many1 P.digit
  ; return (IntLit $ read dec, pos) } <?> "Integer Literal"

-- | eat a token
eat :: Token -> TokenParser ()
eat tok = P.token show pTokenPos test
  where test ptok = if tok == fst ptok then Just () else Nothing

getString :: TokenParser String
getString = P.token show pTokenPos strip
  where strip (StrLit s, _) = Just s
        strip (Ident s, _) = Just s
        strip _ = Nothing

getInt :: TokenParser Int
getInt = P.token show pTokenPos strip
  where strip (IntLit i, _) = Just i
        strip _ = Nothing

tokenize :: P.SourceName -> Text -> Either P.ParseError [PToken]
tokenize = P.parse tokenizer
