module Printing ( showTerms ) where

import Syntax
import Context
import Typing
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

{- --------------------------------
   Printing Types
   -------------------------------- -}

showType :: Ty -> ContextThrowsError String
showType TyBool = return "Bool"
showType TyNat  = return "Nat"
showType (TyArr ty1 ty2) 
    = liftM2 (\x y -> x ++ " -> " ++ y) (showType ty1) (showType ty2)


{- --------------------------------
   Printing Terms
   -------------------------------- -}

showTerm :: Term -> ContextThrowsError String
showTerm TmTrue  = return "true"
showTerm TmFalse = return "false"
showTerm TmZero  = return "0"
showTerm (TmSucc t) | isnumericval t = return $ show $ countSucc 1 t
                    | otherwise      = liftM (\s -> "(succ "   ++ s ++ ")") $ 
                                       showTerm t
                    where countSucc c TmZero = c
                          countSucc c (TmSucc t) = countSucc (c + 1) t
showTerm (TmPred t)   = liftM (\s -> "(pred "   ++ s ++ ")") $ showTerm t 
showTerm (TmIsZero t) = liftM (\s -> "(iszero " ++ s ++ ")") $ showTerm t 
showTerm (TmIf t1 t2 t3) = do t1Shown <- showTerm t1
                              t2Shown <- showTerm t2
                              t3Shown <- showTerm t3
                              return $ "if " ++ t1Shown ++ " then " ++
                                     t2Shown ++ " else " ++ t3Shown
showTerm (TmBind var binding) 
    = do ctx <- get
         put $ appendBinding var binding ctx
         return var
showTerm (TmVar idx ctxLen) 
    = do ctx <- get
         if ctxLength ctx == ctxLen
           then liftThrows $ nameOf idx ctx
           else throwError $ Default "Context length mismatch"
showTerm (TmAbs var ty body)
    = do ctx <- get
         typeShown <- showType ty
         let name = pickFreshName var ctx
         bodyShown <- withBinding name (VarBind ty) $ showTerm body
         return $ "(lambda " ++ name ++ ":" ++ typeShown ++ ". " ++ 
                bodyShown ++ ")"
showTerm (TmApp t1 t2) = liftM2 (\x y -> x ++ " " ++ y) 
                         (showTerm t1) (showTerm t2)

showTerms :: [Term] -> ThrowsError String
showTerms ts = runContextThrows $ concatTerms $ mapM showTermAndType ts
    where showTermAndType t = do termShown <- showTerm t
                                 ty <- typeof t
                                 typeShown <- showType ty
                                 return $ termShown ++ " : " ++ typeShown
          concatTerms = liftM $ concat . map ( ++ "\n")

