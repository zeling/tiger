{-# LANGUAGE DerivingFunctor #-}
module Language.Tiger.TypeChecker.Type
       () where

import Data.Functor.Cata
import Language.Tiger.AST
import qualified Data.Map as M
import Control.Monad.Reader

data Type a = IntTy
            | StringTy
            | FuncTy [Type] Type
            | UnitTy
            | NilTy
            deriving (Eq, Show, Functor)

type Env = M.Map String Type

-- newtype TypeCheck = TypeCheck { runTypeCheck :: Reader Env Type }

-- type Type = Mu TypeF

typeCheck :: Exp -> Maybe Type
typeCheck exp = runReader (cata checkAlg exp) empty

checkAlg :: ExpF -> Reader Env (Maybe Type)
checkAlg (VarExp (SimpleVar v)) = ask >>= return . M.lookup v
checkAlg UnitExp = return $ Just UnitTy
checkAlg NilExp = return $ Just NilTy
checkAlg (IntExp _) = return $ Just IntTy
checkAlg (StringExp _) = return $ Just StringTy
checkAlg (CallExp funcName paramTypes) = ask >>= \env ->
  return $ do { t <- M.lookup funcName env
              ; case t of
              (FuncTy tys ret) -> if tys == paraTypes then return ret else Nothing
              _ -> Nothing }
checkAlg (OpExp t1 op t2) = if t1 == IntExp && t2 == IntExp
                            then return $ Just IntExp
                            else return Nothing
