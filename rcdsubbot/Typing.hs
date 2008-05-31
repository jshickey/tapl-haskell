{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import SimpleContext
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

badApplication = TypeMismatch "Invalid argument passed to an abstraction"
notAbstraction = TypeMismatch "First term of application must be an abstraction"
expectedBool = TypeMismatch "The conditional of an if-statement must be a Bool"
ifMismatch = TypeMismatch "Predicate and alternative of an if-statement must be of the same type"
projError = TypeMismatch "A projection can only be applied to a record"

checkType :: Term -> Ty -> Ty -> ContextThrowsError Ty
checkType t expected output
    = do tyT <- typeof t
         if tyT == expected
           then return output
           else throwError $ TypeMismatch $ "Expected " ++ show expected ++
                ", but saw " ++ show tyT

{- -------------------------------------
   typeof
 ------------------------------------- -}

typeof :: Term -> ContextThrowsError Ty
typeof (TmVar idx _) = do ctx <- get
                          b <- liftThrows $ bindingOf idx ctx
                          liftThrows $ typeOfBinding b
typeof (TmAbs var ty body) = withBinding var (VarBind ty) $ 
                             liftM (TyArr ty) $ typeof body 
typeof (TmApp t1 t2) 
    = do tyT1 <- typeof t1
         tyT2 <- typeof t2
         case tyT1 of
           (TyArr _ _) -> checkTyArr tyT1 tyT2
           otherwise -> throwError notAbstraction
    where checkTyArr (TyArr tyArr1 tyArr2) tyT2
              | tyArr1 == tyT2 = return tyArr2
              | otherwise      = throwError badApplication 
typeof (TmRecord fs) = liftM TyRecord $ mapM typeofField fs
    where typeofField (n,t) = do ty <- typeof t
                                 return (n, ty)
typeof (TmProj r name) = do recordTy <- typeof r
                            case recordTy of
                              TyRecord fs -> accessField name fs
                              otherwise -> throwError projError

accessField name [] = throwError $ TypeMismatch $ "No field " ++ name
accessField name ((n,t):fs) | n == name = return t
                            | otherwise = accessField name fs

{- -------------------------------------
   typeofBinding
 ------------------------------------- -}

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty
