{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Language.Tiger.AST.Declaration
       ( ExpF(..)
       , Dec(..)
       , Ty(..)
       , Var(..)
       , Op(..)
       , Field
       , Exp ) where

import Data.Functor.Cata (Mu)

type Field = (String, String)

data Dec = FunctionDec String [Field] (Maybe String) Exp
         | VarDec String (Maybe String) Exp
         | TypeDec String Ty

data Ty = NameTy String
        | RecordTy [Field]
        | ArrayTy String

data Var = SimpleVar String
         | FieldVar Var String
         | SubscriptVar Var Exp

data ExpF a = VarExp Var
            | NilExp
            | IntExp Int
            | StringExp String
            | CallExp String [a]
            | OpExp a Op a
            | RecordExp String [(String, a)]
            | SeqExp [a]
            | AssignExp Var a
            | IfExp a a (Maybe a)
            | WhileExp a a
            | ForExp Var a a a
            | BreakExp
            | LetExp [Dec] [a]
            | ArrayExp String a a

type Exp = Mu ExpF

data Op = PlusOp | MinusOp | MultOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp | AndOp | OrOp

deriving instance Show Var
deriving instance Show Dec
deriving instance Show Ty
deriving instance Show Op
deriving instance Eq Var
deriving instance Eq Dec
deriving instance Eq Ty
deriving instance Eq Op
deriving instance Functor ExpF
deriving instance (Show a) => Show (ExpF a)
deriving instance (Eq a) => Eq (ExpF a)
