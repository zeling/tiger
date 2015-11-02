{-# LANGUAGE DeriveFunctor #-}

module Language.Tiger.Parser.Grammar where

import Language.Tiger.AST
import Language.Tiger.Lexer
import Data.Functor.Cata
import qualified Text.Parsec as P
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Text.Parsec ((<|>), (<?>))

data AccessF a = Subscript Exp a
               | Field String a
               | None
               deriving (Show, Functor)

type Access = Mu AccessF

lvalAlg :: Algebra AccessF (Var -> Var)
lvalAlg (Subscript exp var) = \v -> var $ SubscriptVar v exp
lvalAlg (Field field var) = \v -> var $ FieldVar v field
lvalAlg None = id

expression :: TokenParser Exp
expression = undefined

dec :: TokenParser Dec
dec = tyDec
      <|> varDec
      <|> funDec

tyDec :: TokenParser Dec
tyDec = TypeDec
        <$> (eat Type *> getIdent)
        <*> (eat Assign *> ty)

varDec :: TokenParser Dec
varDec = VarDec
         <$> (eat Var *> getIdent)
         <*> optionalTypeId
         <*> (eat Assign *> expression)

funDec :: TokenParser Dec
funDec = FunctionDec
         <$> (eat Function *> getIdent)
         <*> (eat LParen *> tyField <* eat RParen)
         <*> optionalTypeId
         <*> (eat Eq *> expression)

ty :: TokenParser Ty
ty = NameTy <$> getIdent
     <|> RecordTy <$> (eat LBrace *> tyField <* eat RBrace)
     <|> ArrayTy <$> (eat Array *> eat Of *> getIdent)

tyField :: TokenParser [Field]
tyField = P.sepBy ((,) <$> getIdent <*> getIdent) (eat Comma)

optionalTypeId :: TokenParser (Maybe String)
optionalTypeId = P.optionMaybe (eat Colon *> getIdent)

lval' :: TokenParser (String, Access)
lval' = (,) <$> getIdent <*> lvalAcc

lvalAcc :: TokenParser Access
lvalAcc = Mu <$> (P.option None $
          Field <$> (eat Dot *> getIdent) <*> lvalAcc
          <|> Subscript <$> (eat LBrack *> expression <* eat RBrack) <*> lvalAcc)

lval :: TokenParser Var
lval = toVar <$> lval'
       where toVar (s, a) = cata lvalAlg a $ (SimpleVar s)

infixExp :: TokenParser Exp
infixExp =  buildExpressionParser operators termExp
           where operators =
                   [ [ op Mult MultOp AssocLeft, op Div DivideOp AssocLeft ]
                   , [ op Plus PlusOp AssocLeft, op Minus MinusOp AssocLeft ]
                   , [ op Eq EqOp AssocNone, op Neq NeqOp AssocNone
                     , op Le LeOp AssocNone, op Lt LtOp AssocNone
                     , op Ge GeOp AssocNone, op Gt GtOp AssocNone ]
                   , [ op And AndOp AssocRight ]
                   , [ op Or OrOp AssocRight ] ]
                 op tok o assoc = Infix (eat tok *> pure ((flip $ ((.).(.).(.)) Mu OpExp) o)) assoc


termExp :: TokenParser Exp
termExp = P.choice
            [ intLiteral
            , (eat LParen *> expression <* eat RParen)
            , (Mu . VarExp) <$> lval ]

intLiteral :: TokenParser Exp
intLiteral = Mu <$> (IntExp <$> getInt)

stringLiteral :: TokenParser Exp
stringLiteral = Mu <$> (StringExp <$> getString)

literal :: TokenParser Exp
literal = intLiteral <|> stringLiteral
