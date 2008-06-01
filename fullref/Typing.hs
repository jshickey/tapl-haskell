{- Provides the methods for determining the type of a term or a binding
 -}
module Typing where

import Syntax
import SimpleContext
import Store
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State

checkType :: Term -> Ty -> Ty -> StoreContextThrows Ty
checkType t expected output
    = do tyT <- typeof t
         if tyT == expected
           then return output
           else throwError $ TypeMismatch $ "Expected " ++ show expected ++
                ", but saw " ++ show tyT

{- -------------------------------------
   typeof
 ------------------------------------- -}

typeof :: Term -> StoreContextThrows Ty
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
                                   return $ joinTypes tyC tyA
typeof (TmBind v TyVarBind) = return $ TyId v
typeof (TmBind v b) = do lift $ modify $ appendBinding v b
                         liftThrowsToStore $ typeOfBinding b
typeof (TmAscribe t ty) = do tyT <- typeof t
                             if subtype tyT ty
                               then return ty
                               else throwError ascribeError
typeof (TmVar idx _) = do ctx <- lift get
                          b <- liftThrowsToStore $ bindingOf idx ctx
                          liftThrowsToStore $ typeOfBinding b
typeof (TmAbs var ty body) = withBindingUnderStore var (VarBind ty) $ 
                             liftM (TyArr ty) $ typeof body 
typeof (TmLet var t body)  = do ty <- typeof t
                                withBindingUnderStore var (VarBind ty) $ typeof body
typeof (TmApp t1 t2) 
    = do tyT1 <- typeof t1
         tyT2 <- typeof t2
         case tyT1 of
           (TyArr _ _) -> checkTyArr tyT1 tyT2
           (TyVar _)   -> return tyT2
           TyBot       -> return TyBot
           otherwise -> throwError notAbstraction
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
                        TyArr t1 t2 | subtype t2 t1 -> return t2
                        otherwise -> throwError fixError
typeof (TmRef t) = do s <- get
                      ty <- typeof t
                      let (_,s') = storeRef s t ty
                      put s'
                      return $ TyRef ty
typeof (TmDeref t) = do refTy <- typeof t
                        case refTy of
                          TyRef ty -> return ty
                          otherwise -> throwError $ TypeMismatch "deref of non-ref"
typeof (TmAssign _ _) = return TyUnit
typeof ty = throwError $ Default $ "Unknown type: " ++ show ty

accessField name [] = throwError $ TypeMismatch $ "No field " ++ name
accessField name ((n,t):fs) | n == name = return t
                            | otherwise = accessField name fs

typeofTerms :: [Term] -> ThrowsError [Ty]
typeofTerms = runStoreContextThrows . mapM typeof

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
subtype (TyRef ty1) (TyRef ty2)
                = (subtype ty1 ty2) && (subtype ty2 ty1)
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

{- -------------------------------------
   join/meet
 ------------------------------------- -}


joinTypes :: Ty -> Ty -> Ty
joinTypes TyTop _      = TyTop
joinTypes _     TyTop  = TyTop
joinTypes TyBot ty     = ty
joinTypes ty    TyBot  = ty
joinTypes (TyArr ty11 ty12) (TyArr ty21 ty22) 
    = TyArr (meetTypes ty11 ty21) (joinTypes ty12 ty22)
joinTypes (TyRecord fs1) (TyRecord fs2) = TyRecord $ recur fs1
    where recur [] = []
          recur ((i,ty1):fs) = case lookup i fs2 of
                                 Just ty2 -> (i, joinTypes ty1 ty2):(recur fs)
                                 Nothing  -> recur fs
-- possible extension: source and sink when ty1 and ty2 are not
-- subtypes of each other, which the OCaml implementation has a
-- a partial implementation of
joinTypes (TyRef ty1) (TyRef ty2) 
    | (subtype ty1 ty2) && (subtype ty2 ty1) = TyRef ty2
joinTypes ty1 ty2 | ty1 == ty2 = ty1
                  | otherwise  = TyTop

meetTypes :: Ty -> Ty -> Ty
meetTypes TyBot _      = TyBot
meetTypes _     TyBot  = TyBot
meetTypes TyTop ty     = ty
meetTypes ty    TyTop  = TyTop
meetTypes (TyArr ty11 ty12) (TyArr ty21 ty22) 
    = TyArr (joinTypes ty11 ty21) (meetTypes ty12 ty22)
meetTypes (TyRecord fs1) (TyRecord fs2) = TyRecord $ recur fs1
    where recur [] = []
          recur ((i,ty1):fs) = case lookup i fs2 of
                                 Just ty2 -> (i, meetTypes ty1 ty2):(recur fs)
                                 Nothing  -> recur fs
meetTypes (TyRef ty1) (TyRef ty2) 
    | (subtype ty1 ty2) && (subtype ty2 ty1) = TyRef ty2
meetTypes ty1 ty2 | ty1 == ty2 = ty1
                  | otherwise  = TyBot 
