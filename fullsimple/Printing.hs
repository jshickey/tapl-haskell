module Printing ( showTerms ) where

import Syntax
import Context
import Typing
import TaplError
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

{- --------------------------------
   Printing Types
   -------------------------------- -}

showType :: Ty -> Printer ()
showType TyBool          = tell "Bool"
showType TyNat           = tell "Nat"
showType TyUnit          = tell "Unit"
showType TyString        = tell "String"
showType (TyArr ty1 ty2) = showType ty1 >> tell " -> " >> showType ty2

{- --------------------------------
   Printing a single Term
   -------------------------------- -}

showTerm :: Term -> Printer ()
showTerm TmTrue  = tell "true"
showTerm TmFalse = tell "false"
showTerm TmZero  = tell "0"
showTerm TmUnit  = tell "unit"
showTerm (TmString str) = tell $ "\"" ++ str ++ "\""
showTerm (TmSucc t) | isnumericval t = tell $ show $ countSucc 1 t
                    | otherwise      = showOneArg "succ" t
                    where countSucc c TmZero     = c
                          countSucc c (TmSucc t) = countSucc (c + 1) t
showTerm (TmPred t)      = showOneArg "pred" t
showTerm (TmIsZero t)    = showOneArg "iszero" t
showTerm (TmIf t1 t2 t3) = tell "if "   >> showTerm t1 >>
                           tell "then " >> showTerm t2 >>
                           tell "else " >> showTerm t3
showTerm (TmBind var binding) = modify (appendBinding var binding) >> tell var
showTerm (TmVar idx ctxLen) 
    = do ctx <- get
         if ctxLength ctx == ctxLen
           then do name <- liftThrowsToPrinter $ nameOf idx ctx
                   tell name
           else throwError $ Default "Context length mismatch"
showTerm (TmAbs var ty body)
    = do ctx <- get
         let name = pickFreshName var ctx
         tell $ "(lambda " ++ name ++ ":"
         showType ty
         tell ". "
         withBinding name (VarBind ty) $ showTerm body
         tell ")"
showTerm (TmApp t1 t2) = showTerm t1 >> tell " " >> showTerm t2

{- --------------------------------
   Printing a list of Terms
   -------------------------------- -}

showTerms :: [Term] -> ThrowsError String
showTerms = runPrinter . mapM_ showLine 
    where showLine t = showTerm t >> 
                       tell " : " >> 
                       (lift (typeof t) >>= showType) >> 
                       tell "\n"

{- --------------------------------
   Helpers
   -------------------------------- -}

showOneArg :: String -> Term -> Printer ()
showOneArg name t = tell "(" >> tell name >> tell " " >>
                    showTerm t >>
                    tell ")"

-- Pile on another monad transformer to create a monad that 
-- encapsulates:
--    (1) accumulating a string
--    (2) a Context (the state)
--    (3) the possibility of errors (ThrowsError)

type Printer = WriterT String ContextThrowsError

runPrinter :: Printer () -> ThrowsError String
runPrinter = runContextThrows . execWriterT 

liftThrowsToPrinter :: ThrowsError a -> Printer a
liftThrowsToPrinter = lift . liftThrows
