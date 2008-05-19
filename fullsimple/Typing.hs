{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import Context
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

badApplication = TypeMismatch "Invalid argument passed to an abstraction"
notAbstraction = TypeMismatch "First term of application must be an abstraction"
expectedBool = TypeMismatch "The conditional of an if-statement must be a Bool"
ifMismatch = TypeMismatch "Predicate and alternative of an if-statement must be of the same type"

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
typeof TmTrue  = return TyBool
typeof TmFalse = return TyBool
typeof TmZero  = return TyNat
typeof TmUnit  = return TyUnit
typeof (TmFloat _)  = return TyFloat
typeof (TmTimesFloat t1 t2) = checkType t1 TyFloat TyFloat >>
                              checkType t2 TyFloat TyFloat
typeof (TmString _) = return TyString
typeof (TmSucc t)   = checkType t TyNat TyNat
typeof (TmPred t)   = checkType t TyNat TyNat
typeof (TmIsZero t) = checkType t TyNat TyBool
typeof (TmIf p c a) = do tyP <- typeof p
                         if tyP /= TyBool
                           then throwError expectedBool
                           else do tyC <- typeof c
                                   tyA <- typeof a
                                   if tyC == tyA
                                     then return tyC
                                     else throwError ifMismatch
typeof (TmBind _ b) = liftThrows $ typeOfBinding b
typeof (TmVar idx _) = do ctx <- get
                          b <- liftThrows $ bindingOf idx ctx
                          liftThrows $ typeOfBinding b
typeof (TmAbs var ty body) = withBinding var (VarBind ty) $ 
                             liftM (TyArr ty) $ typeof body 
typeof (TmApp t1 t2) 
    = do tyT1 <- typeof t1
         tyT2 <- typeof t2
         case tyT1 of
           (TyArr tyArr1 tyArr2) | tyArr1 == tyT2 -> return tyArr2
                                 | otherwise      -> throwError badApplication 
           otherwise -> throwError notAbstraction

{- -------------------------------------
   typeofBinding
 ------------------------------------- -}

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty
typeOfBinding _ = throwError $ Default "No type information exists"