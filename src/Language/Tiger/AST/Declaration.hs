{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
module Language.Tiger.AST.Declaration where
newtype Mu f = In (f (Mu f))


type Field = (Symbol, Symbol)

data Dec = FunctionDec (Symbol, [Field], Maybe Symbol, Exp)
         | VarDec Symbol (Maybe Symbol) Exp
         | TypeDec Symbol Ty

data Ty = NameTy Symbol
        | RecordTy [Field]
        | ArrayTy Symbol

type Symbol = String

data Var = SimpleVar Symbol
         | FieldVar Var Symbol
         | SubscriptVar Var Exp

data ExpF a = VarExp Var
            | NilExp
            | IntExp Int
            | StringExp String
            | CallExp Symbol [a]
            | OpExp a Op a
            | RecordExp [(Symbol, a)] Symbol
            | SeqExp [a]
            | AssignExp Var a
            | IfExp a a (Maybe a)
            | WhileExp a a
            | ForExp Var a a a
            | BreakExp
            | LetExp [Dec] a
            | ArrayExp Symbol a a

type Exp = Mu ExpF

data Op = PlusOp | MinusOp | MultOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

deriving instance Show Var
deriving instance Show Dec
deriving instance Show Ty
deriving instance Show Op
deriving instance Eq Var
deriving instance Eq Dec
deriving instance Eq Ty
deriving instance Eq Op
deriving instance (Show (f (Mu f))) => Show (Mu f)
deriving instance (Ord (f (Mu f))) => Ord (Mu f)
deriving instance (Eq (f (Mu f))) => Eq (Mu f)
deriving instance (Read (f (Mu f))) => Read (Mu f)
deriving instance Functor ExpF
deriving instance (Show a) => Show (ExpF a)
deriving instance (Eq a) => Eq (ExpF a)
