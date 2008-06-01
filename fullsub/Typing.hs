{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import SimpleContext
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

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
typeof (TmBind v TyVarBind) = return $ TyId v
typeof (TmBind v b) = do modify $ appendBinding v b
                         liftThrows $ typeOfBinding b
typeof (TmAscribe t ty) = checkType t ty ty
typeof (TmVar idx _) = do ctx <- get
                          b <- liftThrows $ bindingOf idx ctx
                          liftThrows $ typeOfBinding b
typeof (TmAbs var ty body) = withBinding var (VarBind ty) $ 
                             liftM (TyArr ty) $ typeof body 
typeof (TmLet var t body)  = do ty <- typeof t
                                withBinding var (VarBind ty) $ typeof body
typeof (TmApp t1 t2) 
    = do tyT1 <- typeof t1
         tyT2 <- typeof t2
         case tyT1 of
           (TyArr _ _) -> checkTyArr tyT1 tyT2
           (TyVar _)   -> return tyT2
           TyBot       -> return TyBot
           otherwise   -> throwError notAbstraction
    where checkTyArr (TyArr tyArr1 tyArr2) tyT2
              | subtype tyT2 tyArr1 = return tyArr2
              | otherwise           = throwError badApplication 
typeof (TmRecord fs) = liftM TyRecord $ mapM typeofField fs
    where typeofField (n,t) = do ty <- typeof t
                                 return (n, ty)
typeof (TmProj r name) = do recordTy <- typeof r
                            case recordTy of
                              TyRecord fs -> accessField name fs
                              otherwise -> throwError projError
typeof (TmTag _ _ ty) = return ty
typeof (TmCase t ((label,_):cs)) = do (TyVariant fs) <- typeof t
                                      accessField label fs
typeof (TmInert ty) = return ty
typeof (TmFix t) = do ty <- typeof t
                      case ty of
                        TyArr t1 t2 | t1 == t2 -> return t1
                        otherwise -> throwError $ TypeMismatch "bad fix type"
typeof _ = throwError $ Default "Unknown type"

accessField name [] = throwError $ TypeMismatch $ "No field " ++ name
accessField name ((n,t):fs) | n == name = return t
                            | otherwise = accessField name fs

typeofTerms :: [Term] -> ThrowsError [Ty]
typeofTerms = runContextThrows . mapM typeof

{- -------------------------------------
   subtype -- check whether first arg is 
   a subtype of the second arg
 ------------------------------------- -}

subtype :: Ty -> Ty -> Bool
-- to check for records, we handle both S-RcdWidth and S-RcdPerm
-- by checking that each field of the prospective subtype record
-- matches a field of the supertype record.  We handle S-RcdDepth
-- by checking that the field it matching up with is a supertype
subtype (TyRecord fs1) (TyRecord fs2)
                = and $ map matching fs2
    where matching (i,ty2) = case lookup i fs1 of
                              Just ty1 -> subtype ty1 ty2
                              Nothing  -> False
subtype (TyArr ty11 ty12) (TyArr ty21 ty22)
                = (subtype ty21 ty11) && (subtype ty12 ty22)
subtype TyBot _ = True
subtype _ TyTop = True
subtype ty1 ty2 = ty1 == ty2

{- -------------------------------------
   typeofBinding
 ------------------------------------- -}

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty
typeOfBinding (TmAbbBind _ (Just ty)) = return ty
typeOfBinding (TyAbbBind ty) = return ty
typeOfBinding _ = throwError $ Default "No type information exists"