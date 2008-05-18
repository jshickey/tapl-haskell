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

isval :: Term -> Bool
isval TmTrue = True
isval TmFalse = True
isval TmZero = True
isval (TmAbs _ _ _) = True
isval _ = False

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

badApplication = TypeMismatch "Invalid argument passed to an abstraction"
notAbstraction = TypeMismatch "First term of application must be an abstraction"

typeof :: Term -> ContextThrowsError Ty
typeof TmTrue = return TyBool
typeof TmFalse = return TyBool
typeof TmZero = return TyNat
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
           otherwise                              -> throwError notAbstraction

showType :: Ty -> ContextThrowsError String
showType TyBool = return "Bool"
showType TyNat  = return "Nat"
showType (TyArr ty1 ty2) 
    = liftM2 (\x y -> x ++ " -> " ++ y) (showType ty1) (showType ty2)

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

pickFreshName :: String -> Context -> String
pickFreshName var (Ctx ps) = iter var ps
    where iter name [] = name
          iter name ((n,_):rest) | n == name = iter (name ++ "'") ps
                                 | otherwise = iter name rest

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
      else if idx < 0
           then throwError $ Default "Negative index for context"
           else return $ ps !! idx

appendBinding :: String -> Binding -> Context -> Context
appendBinding var binding (Ctx ps) = Ctx $ (var,binding) : ps


type ContextThrowsError = ErrorT TaplError (State Context)

liftThrows :: ThrowsError a -> ContextThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runContextThrows :: ContextThrowsError a -> ThrowsError a
runContextThrows action = evalState (runErrorT action) newContext

-- helper function to perform an action with the 
-- (temporary) addition of a binding
withBinding var b action = do ctx <- get
                              put $ appendBinding var b ctx
                              result <- action
                              put ctx
                              return result
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

