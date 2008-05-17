module Syntax where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import TaplError    

{- --------------------------------
   TERMS
   -------------------------------- -}

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmCase Term [(String, (String, Term))]
          | TmTag String Term Ty
          | TmVar Int Int
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmLet String Term Term
          | TmFix Term
          | TmString String
          | TmUnit
          | TmAscribe Term Ty
          | TmRecord [(String, Term)]
          | TmProj Term String
          | TmFloat Double
          | TmTimesfloat Term Term
          | TmZero
          | TmSucc Term
          | TmPred
          | TmIsZero Term
          | TmInert Ty
          | TmBind String Binding
          deriving (Show, Eq)

{- --------------------------------
   TYPES
   -------------------------------- -}

data Ty = TyVar Int Int
        | TyId String
        | TyArr Ty Ty -- "t1 -> t2"
        | TyRecord [(String, Ty)]
        | TyVariant [(String, Ty)]
        | TyBool
        | TyString
        | TyFloat
        | TyNat
          deriving (Show, Eq)

typeof :: Term -> ContextThrowsError Ty
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmZero = return TyNat
typeof (TmBind _ b) = liftThrows $ typeOfBinding b
typeof (TmVar idx _) = do ctx <- get
                          b <- liftThrows $ bindingOf idx ctx
                          liftThrows $ typeOfBinding b

showType :: Ty -> ContextThrowsError String
showType TyBool = return "Bool"
showType TyNat  = return "Nat"

{- --------------------------------
   CONTEXT & BINDING

   de Bruijn indexes are used.  
   -------------------------------- -}

data Binding = NameBind
             | TyVarBind
             | VarBind Ty
             | TmAbbBind Term (Maybe Ty)
             | TyAbbBind Ty
               deriving (Show, Eq)

typeOfBinding :: Binding -> ThrowsError Ty
typeOfBinding (VarBind ty) = return ty
typeOfBinding _ = throwError $ Default "No type information exists"

newtype Context = Ctx [(String, Binding)]
    deriving (Show, Eq)

newContext :: Context
newContext = Ctx []

ctxLength :: Context -> Int
ctxLength (Ctx ps) = length ps

indexOf :: String -> Context -> ThrowsError Int
indexOf var (Ctx ps) = iter 0 ps
    where iter _ [] = throwError $ UndefinedVariable var
          iter i ((v,b):ps) | v == var  = return i
                            | otherwise = iter (i + 1) ps

nameOf :: Int -> Context -> ThrowsError String
nameOf idx = (liftM fst) . (bindingPairOf idx)

bindingOf :: Int -> Context -> ThrowsError Binding
bindingOf idx = (liftM snd) . (bindingPairOf idx)

bindingPairOf :: Int -> Context -> ThrowsError (String, Binding)
bindingPairOf idx (Ctx ps) 
    = if idx >= length ps
      then throwError $ UndefinedVariable $ "at index " ++ show idx
      else return $ ps !! idx

appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var,binding) : ps


type ContextThrowsError = ErrorT TaplError (State Context)

liftThrows :: ThrowsError a -> ContextThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runContextThrows :: ContextThrowsError a -> ThrowsError a
runContextThrows action = evalState (runErrorT action) newContext

{- --------------------------------
   PRINTING
   -------------------------------- -}

showTerm :: Term -> ContextThrowsError String
showTerm TmTrue = return "true"
showTerm TmFalse = return "false"
showTerm TmZero = return "0"
showTerm (TmBind var binding) 
    = do ctx <- get
         put $ appendBinding var binding ctx
         return var
showTerm (TmVar idx ctxLen) 
    = do ctx <- get
         if ctxLength ctx == ctxLen
           then liftThrows $ nameOf idx ctx
           else throwError $ Default "Context length mismatch"

showTerms :: [Term] -> ThrowsError String
showTerms ts = runContextThrows $ concatTerms $ mapM showTermAndType ts
    where showTermAndType t = do termShown <- showTerm t
                                 ty <- typeof t
                                 typeShown <- showType ty
                                 return $ termShown ++ " : " ++ typeShown
          concatTerms = liftM $ concat . map ( ++ "\n")

