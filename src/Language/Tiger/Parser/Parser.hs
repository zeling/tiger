{-# LANGUAGE DeriveFunctor #-}

module Language.Tiger.Parser.Grammar where

import Language.Tiger.AST
import Language.Tiger.Lexer
import Data.Functor.Cata
import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))

data AccessF a = Subscript Exp a
             | Field String a
             | None
             deriving (Show, Functor)

type Access = Mu AccessF

lvalAlg :: String -> Algebra AccessF Var
lvalAlg s (Subscript exp var) = SubscriptVar var exp
lvalAlg s (Field field var) = FieldVar var field
lvalAlg s None = SimpleVar s

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

lval :: TokenParser Access
lval = undefined

lval' :: TokenParser Access
lval' = undefined

expression :: TokenParser exp
expression = undefined
