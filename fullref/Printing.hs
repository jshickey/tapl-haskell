module Printing ( showTerms ) where

import Syntax
import Store
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
showType TyBool             = tell "Bool"
showType TyNat              = tell "Nat"
showType TyUnit             = tell "Unit"
showType TyString           = tell "String"
showType TyFloat            = tell "Float"
showType TyTop              = tell "Top"
showType TyBot              = tell "Bot"
showType (TyArr ty1 ty2)    = case ty1 of
                                TyArr _ _ -> tell "(" >> showType ty1 >>
                                             tell ") -> " >> showType ty2
                                otherwise -> showType ty1 >> tell " -> " >> 
                                             showType ty2
showType (TyId str)         = tell str
showType (TyRecord [])      = tell "{}"
showType (TyRecord (f:fs))  = tell "{" >> showField f >> 
                              mapM_ (\f -> tell ", " >> showField f) fs >>
                              tell "}"
    where showField (n,ty) | isnumber n = showType ty
                           | otherwise  = tell (n ++ ":") >> showType ty
showType (TyVariant [])     = tell "<>"
showType (TyVariant (f:fs)) = tell "<" >> showField f >>
                              mapM_ (\f -> tell ", " >> showField f) fs >>
                              tell ">"
    where showField (n,ty) = tell (n ++ ":") >> showType ty
showType (TyVar (TmVar idx ctxLen)) = showVar idx ctxLen
showType (TyRef ty) = tell "Ref " >> showType ty

{- --------------------------------
   Printing a single Term
   -------------------------------- -}

showTerm :: Term -> Printer ()
showTerm TmTrue  = tell "true"
showTerm TmFalse = tell "false"
showTerm TmZero  = tell "0"
showTerm TmUnit  = tell "unit"
showTerm (TmFloat val) = tell $ show val
showTerm (TmTimesFloat t1 t2) = tell "(timesfloat " >>
                                showTerm t1 >> tell " " >>
                                showTerm t2 >> tell ")"
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
showTerm (TmAscribe t ty) = showTerm t >> tell " as " >> showType ty
showTerm (TmBind var binding) = lift2 (modify (appendBinding var binding)) >> 
                                case binding of
                                  TyAbbBind _ -> tell $ var ++ " :: *"
                                  otherwise   -> tell var
showTerm (TmVar idx ctxLen) = showVar idx ctxLen
showTerm (TmAbs var ty body)
    = do ctx <- lift2 get
         let name = pickFreshName var ctx
         tell $ "(lambda " ++ name ++ ":"
         showType ty
         tell ". "
         withBindingUnderPrinter name (VarBind ty) $ showTerm body
         tell ")"
showTerm (TmLet var t body)
    = do ctx <- lift2 get
         let name = pickFreshName var ctx
         tell $ "let " ++ name ++ " = "
         showTerm t
         tell " in "
         withBindingUnderPrinter name NameBind $ showTerm body
showTerm (TmApp t1 t2) = case t2 of
                           TmApp _ _ -> showTerm t1 >> tell " (" >> 
                                        showTerm t2 >> tell ")"
                           otherwise -> showTerm t1 >> tell " " >> showTerm t2
showTerm (TmRecord [])     = tell "{}"
showTerm (TmRecord (f:fs)) = tell "{" >> showField f >>
                             mapM_ (\f -> tell ", " >> showField f) fs >>
                             tell "}"
    where showField (n,t) | isnumber n = showTerm t
                          | otherwise  = tell (n ++ "=") >> showTerm t
showTerm (TmCase t (c:cs)) = tell "(case " >> showTerm t >> tell " of " >>
                             showBranch c >> showBranches cs >> tell ")"
    where showBranches []     = return ()
          showBranches (c:cs) = tell " | " >> showBranch c >> showBranches cs
          showBranch (label, (var, term)) 
              = do tell ("<" ++ label ++ "=" ++
                         var ++ "> ==> ")
                   withBindingUnderPrinter var NameBind $ showTerm term
showTerm (TmTag var t ty) = tell ("<" ++ var ++ "=") >>
                            showTerm t >>
                            tell "> as " >>
                            showType ty
showTerm (TmProj t name) = showTerm t >> tell ("." ++ name)
showTerm (TmInert ty) = tell "inert[" >> showType ty >> tell "]"
showTerm t = tell $ show t

{- --------------------------------
   Printing a list of Terms
   -------------------------------- -}

showTerms :: [Term] -> [Ty] -> ThrowsError String
showTerms ts = runPrinter . mapM_ showLine . zip ts
    where showLine (t,ty) = showTerm t >> 
                            showTypeOfTerm t ty >>
                            tell "\n"

-- we need to special handling for binders, but otherwise
-- we just use the type that was passed in
showTypeOfTerm :: Term -> Ty -> Printer ()
showTypeOfTerm (TmBind _ (TyAbbBind _)) _ = return () 
showTypeOfTerm (TmBind var TyVarBind)   _ = return ()
showTypeOfTerm _ ty                       = tell " : " >> showType ty

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

type Printer = WriterT String StoreContextThrows

runPrinter :: Printer () -> ThrowsError String
runPrinter = runStoreContextThrows . execWriterT 

liftThrowsToPrinter :: ThrowsError a -> Printer a
liftThrowsToPrinter = lift2 . liftThrows

-- for printing the name of a TyVar or a TmVar
showVar :: Int -> Int -> Printer ()
showVar idx ctxLen = do ctx <- lift2 get
                        if ctxLength ctx == ctxLen
                          then do name <- liftThrowsToPrinter $ nameOf idx ctx
                                  tell name
                          else throwError $ Default $ "Context length mismatch: " ++ "var had " ++ show ctxLen ++ ", but the context length was " ++ show (ctxLength ctx)

isnumber :: String -> Bool                               
isnumber n = elem n $ map show [0..9]

withBindingUnderPrinter var b action = do ctx <- lift2 get
                                          lift2 $ put $ appendBinding var b ctx
                                          result <- action
                                          lift2 $ put ctx
                                          return result

lift2 = lift . lift