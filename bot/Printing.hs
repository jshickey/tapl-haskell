module Printing ( showTerms ) where

import Syntax
import SimpleContext
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
showType TyTop              = tell "Top"
showType TyBot              = tell "Bot"
showType (TyArr ty1 ty2)    = case ty1 of
                                TyArr _ _ -> tell "(" >> showType ty1 >>
                                             tell ") -> " >> showType ty2
                                otherwise -> showType ty1 >> tell " -> " >> 
                                             showType ty2

{- --------------------------------
   Printing a single Term
   -------------------------------- -}

showTerm :: Term -> Printer ()
showTerm (TmVar idx ctxLen) = showVar idx ctxLen
showTerm (TmAbs var ty body)
    = do ctx <- get
         let name = pickFreshName var ctx
         tell $ "(lambda " ++ name ++ ":"
         showType ty
         tell ". "
         withBinding name (VarBind ty) $ showTerm body
         tell ")"
showTerm (TmApp t1 t2) = case t2 of
                           TmApp _ _ -> showTerm t1 >> tell " (" >> 
                                        showTerm t2 >> tell ")"
                           otherwise -> showTerm t1 >> tell " " >> showTerm t2

{- --------------------------------
   Printing a list of Terms
   -------------------------------- -}

showTerms :: [Term] -> [Ty] -> ThrowsError String
showTerms ts tys = runPrinter  $ mapM_ showLine $ zip ts tys
    where showLine (t,ty) = showTerm t >> 
                            tell " : " >>
                            showType ty >>
                            tell "\n"

{- --------------------------------
   Helpers
   -------------------------------- -}

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

-- for printing the name of a TyVar or a TmVar
showVar :: Int -> Int -> Printer ()
showVar idx ctxLen = do ctx <- get
                        if ctxLength ctx == ctxLen
                          then do name <- liftThrowsToPrinter $ nameOf idx ctx
                                  tell name
                          else throwError $ Default $ "Context length mismatch: " ++ "var had " ++ show ctxLen ++ ", but the context length was " ++ show (ctxLength ctx)

isnumber :: String -> Bool                               
isnumber n = elem n $ map show [0..9]