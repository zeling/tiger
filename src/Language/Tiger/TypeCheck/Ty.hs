{-# LANGUAGE DeriveFunctor #-}
module Language.Tiger.TypeChecker.Type
       () where

import Data.Functor.Cata
import Language.Tiger.AST
import qualified Data.Map as M
import Control.Monad.Reader

data Type = IntTy
          | StringTy
          | FuncTy [Type] Type
          | UnitTy
          | NilTy
          deriving (Eq, Show)

type Env = M.Map String Type
type TypeCheck = Reader Env (Maybe Type)

-- newtype TypeCheck = TypeCheck { runTypeCheck :: Reader Env Type }

-- type Type = Mu TypeF

typeCheck :: Exp -> Maybe Type
typeCheck = extract . cata checkAlg

extract :: TypeCheck -> Maybe Type
extract = flip runReader M.empty

checkAlg :: Algebra ExpF TypeCheck
checkAlg (VarExp (SimpleVar v)) = ask >>= return . M.lookup v
checkAlg UnitExp = return $ Just UnitTy
checkAlg NilExp = return $ Just NilTy
checkAlg (IntExp _) = return $ Just IntTy
checkAlg (StringExp _) = return $ Just StringTy
checkAlg (CallExp funcName rparams) = ask >>= \env ->
  sequence rparams >>= \params ->
  return $ do { t <- M.lookup funcName env
              ; ps <- sequence params
              ; case t of
              (FuncTy tys ret) ->
                if tys == ps then return ret else Nothing
              _ -> Nothing }
checkAlg (OpExp rt1 op rt2) = do { r1 <- rt1
                                 ; r2 <- rt2
                                 ; return $ do { t1 <- r1
                                          ; t2 <- r2
                                          ; if t1 == IntTy && t2 == IntTy
                                            then return IntTy
                                            else Nothing }}
