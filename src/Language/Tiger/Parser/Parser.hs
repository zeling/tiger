module Language.Tiger.Parser.Parser where

import Language.Tiger.AST
import Language.Tiger.Lexer
import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))

type Symbol = String

dec :: TokenParser Dec
dec = tyDec
      <|> varDec
      <|> funDec

tyDec :: TokenParser Dec
tyDec = TypeDec <$> (eat Type *> getString) <*> (eat Assign *> ty)

varDec :: TokenParser Dec
varDec = undefined

funDec :: TokenParser Dec
funDec = undefined

ty :: TokenParser Ty
ty = undefined
