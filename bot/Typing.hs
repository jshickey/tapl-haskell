{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import SimpleContext
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

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
           TyBot       -> return TyBot
           otherwise   -> throwError notAbstraction
    where checkTyArr (TyArr tyArr1 tyArr2) tyT2
              | subtype tyT2 tyArr1 = return tyArr2
              | otherwise           = throwError badApplication 

-- tests whether the first arg is a subtype of the second
subtype :: Ty -> Ty -> Bool
subtype (TyArr ty11 ty12) (TyArr ty21 ty22) 
                = (subtype ty21 ty11) && (subtype ty12 ty22)
subtype TyBot _ = True
subtype _ TyTop = True
subtype ty1 ty2 = ty1 == ty2

typeofTerms :: [Term] -> ThrowsError [Ty]
typeofTerms = runContextThrows . (mapM typeof)

{- -------------------------------------
   typeofBinding
 ------------------------------------- -}

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty

