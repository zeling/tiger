module Language.Tiger.Parser.Grammar where

import Language.Tiger.AST
import Language.Tiger.Lexer
import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))

dec :: TokenParser Dec
dec = tyDec
      <|> varDec
      <|> funDec

tyDec :: TokenParser Dec
tyDec = TypeDec
        <$> (eat Type *> getString)
        <*> (eat Assign *> ty)

varDec :: TokenParser Dec
varDec = VarDec
         <$> (eat Var *> getString)
         <*> optionalTypeId
         <*> (eat Assign *> expression)

funDec :: TokenParser Dec
funDec = FunctionDec
         <$> (eat Function *> getString)
         <*> (eat LParan *> tyField <* eat RParan)
         <*> optionalTypeId
         <*> (eat Eq *> expression)

ty :: TokenParser Ty
ty = NameTy <$> getString
     <|> RecordTy <$> (eat LBrace *> tyField <* eat RBrace)
     <|> ArrayTy <$> (eat Array *> eat Of *> getString)

tyField :: TokenParser [Field]
tyField = P.sepBy ((,) <$> getString <*> getString) (eat Comma)

optionalTypeId :: TokenParser (Maybe String)
optionalTypeId = P.optionMaybe (eat Colon *> getString)

expression :: TokenParser exp
expression = undefined
