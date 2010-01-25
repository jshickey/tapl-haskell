module GenTyping (genTyping) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genTyping :: Config -> IOThrowsError ()
genTyping c =
    lift $ writeToFile "Typing.hs" $ base ++
             (if (useIsorec c) then isorecTypeof else "") ++
             (if (useEquirec c) then equirecTypeof else "") ++
             end ++ rest
    where rest = if (hasSubtypes c)
                 then (subtypeFunc ++ joinFunc)
                 else (dummySubtypeFunc ++ dummyJoinFunc)

base = "{- Provides the methods for determining the type of a term or a binding\n\
\ -}\n\
\module Typing where\n\
\\n\
\import Syntax\n\
\import SimpleContext\n\
\import TaplError\n\
\import Control.Monad\n\
\import Control.Monad.Error\n\
\import Control.Monad.State\n\
\\n\
\checkSubtype :: Term -> Ty -> Ty -> ContextThrowsError Ty\n\
\checkSubtype t expected output\n\
\    = do tyT <- typeof t\n\
\         if subtype tyT expected\n\
\           then return output\n\
\           else throwError $ TypeMismatch $ \"Expected \" ++ show expected ++\n\
\                \", but saw \" ++ show tyT\n\
\\n\
\{- -------------------------------------\n\
\   typeof\n\
\ ------------------------------------- -}\n\
\\n\
\typeof :: Term -> ContextThrowsError Ty\n\
\typeof TmTrue  = return TyBool\n\
\typeof TmFalse = return TyBool\n\
\typeof TmZero  = return TyNat\n\
\typeof TmUnit  = return TyUnit\n\
\typeof (TmFloat _)  = return TyFloat\n\
\typeof (TmTimesFloat t1 t2) = checkSubtype t1 TyFloat TyFloat >>\n\
\                              checkSubtype t2 TyFloat TyFloat\n\
\typeof (TmString _) = return TyString\n\
\typeof (TmSucc t)   = checkSubtype t TyNat TyNat\n\
\typeof (TmPred t)   = checkSubtype t TyNat TyNat\n\
\typeof (TmIsZero t) = checkSubtype t TyNat TyBool\n\
\typeof (TmIf p c a) = do tyP <- typeof p\n\
\                         if tyP /= TyBool\n\
\                           then throwError expectedBool\n\
\                           else do tyC <- typeof c\n\
\                                   tyA <- typeof a\n\
\                                   joinOrThrow tyC tyA\n\
\typeof (TmBind v TyVarBind) = return $ TyId v\n\
\typeof (TmBind v b) = do modify $ appendBinding v b\n\
\                         liftThrows $ typeOfBinding b\n\
\typeof (TmAscribe t ty) = do tyT <- typeof t\n\
\                             if subtype tyT ty\n\
\                               then return ty\n\
\                               else throwError ascribeError\n\
\typeof (TmVar idx _) = do ctx <- get\n\
\                          b <- liftThrows $ bindingOf idx ctx\n\
\                          liftThrows $ typeOfBinding b\n\
\typeof (TmAbs var ty body) = withBinding var (VarBind ty) $ \n\
\                             liftM (TyArr ty) $ typeof body \n\
\typeof (TmLet var t body)  = do ty <- typeof t\n\
\                                withBinding var (VarBind ty) $ typeof body\n\
\typeof (TmApp t1 t2) \n\
\    = do tyT1 <- typeof t1\n\
\         tyT2 <- typeof t2\n\
\         case tyT1 of\n\
\           (TyArr _ _) -> checkTyArr tyT1 tyT2\n\
\           (TyVar _)   -> return tyT2\n\
\           TyBot     -> return TyBot\n\
\           otherwise -> throwError notAbstraction\n\
\    where checkTyArr (TyArr tyArr1 tyArr2) tyT2\n\
\              | subtype tyT2 tyArr1 = return tyArr2\n\
\              | otherwise           = throwError badApplication \n\
\typeof (TmRecord fs) = liftM TyRecord $ mapM typeofField fs\n\
\    where typeofField (n,t) = do ty <- typeof t\n\
\                                 return (n, ty)\n\
\typeof (TmProj r name) = do recordTy <- typeof r\n\
\                            case recordTy of\n\
\                              TyRecord fs -> accessField name fs\n\
\                              otherwise -> throwError projError\n\
\typeof (TmTag _ _ ty) = return ty\n\
\typeof (TmCase t ((label,_):cs)) = do (TyVariant fs) <- typeof t\n\
\                                      accessField label fs\n\
\typeof (TmInert ty) = return ty\n\
\typeof (TmFix t) = do ty <- typeof t\n\
\                      case ty of\n\
\                        TyArr t1 t2 | subtype t2 t1 -> return t2\n\
\                        otherwise -> throwError fixError\n"

isorecTypeof = "typeof (TmFold ty t) =  return ty\n\
\typeof (TmUnfold ty t) = return ty\n"

equirecTypeof = "" -- TODO

end = "typeof _ = throwError $ Default \"Unknown type\"\n\
\\n\
\accessField name [] = throwError $ TypeMismatch $ \"No field \" ++ name\n\
\accessField name ((n,t):fs) | n == name = return t\n\
\                            | otherwise = accessField name fs\n\
\\n\
\typeofTerms :: [Term] -> ThrowsError [Ty]\n\
\typeofTerms = runContextThrows . mapM typeof\n\
\\n\
\{- -------------------------------------\n\
\   typeofBinding\n\
\ ------------------------------------- -}\n\
\\n\
\typeOfBinding :: Binding -> ThrowsError Ty\n\
\typeOfBinding (VarBind ty) = return ty\n\
\typeOfBinding (TmAbbBind _ (Just ty)) = return ty\n\
\typeOfBinding (TyAbbBind ty) = return ty\n\
\typeOfBinding _ = throwError $ Default \"No type information exists\"\n"

dummySubtypeFunc = "\n\
\{- -------------------------------------\n\
\   subtype -- this is a placeholder that\n\
\   will be replaced when we have subtypes\n\
\ ------------------------------------- -}\n\
\\n\
\subtype :: Ty -> Ty -> Bool\n\
\subtype = (==)\n\
\ \n"

dummyJoinFunc = "\n\
\{- -------------------------------------\n\
\   join -- this is a placeholder that\n\
\   will be replaced when we have subtypes \n\
\ ------------------------------------- -}\n\
\\n\
\joinOrThrow :: Ty -> Ty -> ContextThrowsError Ty\n\
\joinOrThrow ty1 ty2 = if ty1 == ty2\n\
\                 then return ty1\n\
\                 else throwError ifMismatch\n\
\ \n"

subtypeFunc = "\n\
\{- -------------------------------------\n\
\   subtype -- check whether first arg is \n\
\   a subtype of the second arg\n\
\ ------------------------------------- -}\n\
\\n\
\subtype :: Ty -> Ty -> Bool\n\
\-- to check for records, we handle both S-RcdWidth and S-RcdPerm\n\
\-- by checking that each field of the prospective subtype record\n\
\-- matches a field of the supertype record.  We handle S-RcdDepth\n\
\-- by checking that the field it matching up with is a supertype\n\
\subtype (TyRecord fs1) (TyRecord fs2)\n\
\                = and $ map matching fs2\n\
\    where matching (i,ty2) = case lookup i fs1 of\n\
\                              Just ty1 -> subtype ty1 ty2\n\
\                              Nothing  -> False\n\
\subtype (TyArr ty11 ty12) (TyArr ty21 ty22)\n\
\                = (subtype ty21 ty11) && (subtype ty12 ty22)\n\
\subtype TyBot _ = True\n\
\subtype _ TyTop = True\n\
\subtype ty1 ty2 = ty1 == ty2\n\
\ \n"
     
joinFunc = "\n\
\{- -------------------------------------\n\
\   join/meet\n\
\ ------------------------------------- -}\n\
\joinOrThrow :: Ty -> Ty -> ContextThrowsError Ty\n\
\joinOrThrow ty1 ty2 = return $ joinTypes ty1 ty2\n\
\\n\
\joinTypes :: Ty -> Ty -> Ty\n\
\joinTypes TyTop _      = TyTop\n\
\joinTypes _     TyTop  = TyTop\n\
\joinTypes TyBot ty     = ty\n\
\joinTypes ty    TyBot  = ty\n\
\joinTypes (TyArr ty11 ty12) (TyArr ty21 ty22) \n\
\    = TyArr (meetTypes ty11 ty21) (joinTypes ty12 ty22)\n\
\joinTypes (TyRecord fs1) (TyRecord fs2) = TyRecord $ recur fs1\n\
\    where recur [] = []\n\
\          recur ((i,ty1):fs) = case lookup i fs2 of\n\
\                                 Just ty2 -> (i, joinTypes ty1 ty2):(recur fs)\n\
\                                 Nothing  -> recur fs\n\
\joinTypes ty1 ty2 | ty1 == ty2 = ty1\n\
\                  | otherwise  = TyTop\n\
\\n\
\meetTypes :: Ty -> Ty -> Ty\n\
\meetTypes TyBot _      = TyBot\n\
\meetTypes _     TyBot  = TyBot\n\
\meetTypes TyTop ty     = ty\n\
\meetTypes ty    TyTop  = TyTop\n\
\meetTypes (TyArr ty11 ty12) (TyArr ty21 ty22) \n\
\    = TyArr (joinTypes ty11 ty21) (meetTypes ty12 ty22)\n\
\meetTypes (TyRecord fs1) (TyRecord fs2) = TyRecord $ recur fs1\n\
\    where recur [] = []\n\
\          recur ((i,ty1):fs) = case lookup i fs2 of\n\
\                                 Just ty2 -> (i, meetTypes ty1 ty2):(recur fs)\n\
\                                 Nothing  -> recur fs\n\
\meetTypes ty1 ty2 | ty1 == ty2 = ty1\n\
\                  | otherwise  = TyBot \n\
\ \n"