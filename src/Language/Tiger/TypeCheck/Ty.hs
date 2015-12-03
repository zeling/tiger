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
type TypeCheck = ReaderT Env Maybe Type

-- newtype TypeCheck = TypeCheck { runTypeCheck :: Reader Env Type }

-- type Type = Mu TypeF

typeCheck :: Exp -> Maybe Type
typeCheck = extract . cata checkAlg

extract :: TypeCheck -> Maybe Type
extract = flip runReaderT M.empty

checkAlg :: Algebra ExpF TypeCheck
checkAlg (VarExp (SimpleVar v)) = ask >>= lift . M.lookup v
checkAlg UnitExp = lift $ Just UnitTy
checkAlg NilExp = lift $ Just NilTy
checkAlg (IntExp _) = lift $ Just IntTy
checkAlg (StringExp _) = lift $ Just StringTy
checkAlg (CallExp name rparams) = do { env <- ask
                                     ; params <- sequence rparams
                                     --; params <- lift $ sequence mparams
                                     ; t <- lift $ M.lookup name env
                                     ; case t of
                                     (FuncTy tys ret) ->
                                       if tys == params
                                       then return ret
                                       else lift Nothing }
checkAlg (OpExp rt1 op rt2) = do { t1 <- rt1
                                  ; t2 <- rt2
                                  ; if t1 == IntTy && t2 == IntTy
                                    then return IntTy
                                    else lift $ Nothing }
