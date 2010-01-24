module GenPrinting (genPrinting) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genPrinting :: Config -> IOThrowsError ()
genPrinting config = lift $ writeToFile "Printing.hs" file

file = "module Printing ( showTerms ) where\n\
\\n\
\import Syntax\n\
\import SimpleContext\n\
\import Typing\n\
\import TaplError\n\
\import Control.Monad\n\
\import Control.Monad.Error\n\
\import Control.Monad.State\n\
\import Control.Monad.Writer\n\
\\n\
\{- --------------------------------\n\
\   Printing Types\n\
\   -------------------------------- -}\n\
\\n\
\showType :: Ty -> Printer ()\n\
\showType TyBool             = tell \"Bool\"\n\
\showType TyNat              = tell \"Nat\"\n\
\showType TyUnit             = tell \"Unit\"\n\
\showType TyString           = tell \"String\"\n\
\showType TyFloat            = tell \"Float\"\n\
\showType TyTop              = tell \"Top\"\n\
\showType TyBot              = tell \"Bot\"\n\
\showType (TyArr ty1 ty2)    = case ty1 of\n\
\                                TyArr _ _ -> tell \"(\" >> showType ty1 >>\n\
\                                             tell \") -> \" >> showType ty2\n\
\                                otherwise -> showType ty1 >> tell \" -> \" >> \n\
\                                             showType ty2\n\
\showType (TyId str)         = tell str\n\
\showType (TyRecord [])      = tell \"{}\"\n\
\showType (TyRecord (f:fs))  = tell \"{\" >> showField f >> \n\
\                              mapM_ (\\f -> tell \", \" >> showField f) fs >>\n\
\                              tell \"}\"\n\
\    where showField (n,ty) | isnumber n = showType ty\n\
\                           | otherwise  = tell (n ++ \":\") >> showType ty\n\
\showType (TyVariant [])     = tell \"<>\"\n\
\showType (TyVariant (f:fs)) = tell \"<\" >> showField f >>\n\
\                              mapM_ (\\f -> tell \", \" >> showField f) fs >>\n\
\                              tell \">\"\n\
\    where showField (n,ty) = tell (n ++ \":\") >> showType ty\n\
\showType (TyVar (TmVar idx ctxLen)) = showVar idx ctxLen\n\
\\n\
\{- --------------------------------\n\
\   Printing a single Term\n\
\   -------------------------------- -}\n\
\\n\
\showTerm :: Term -> Printer ()\n\
\showTerm TmTrue  = tell \"true\"\n\
\showTerm TmFalse = tell \"false\"\n\
\showTerm TmZero  = tell \"0\"\n\
\showTerm TmUnit  = tell \"unit\"\n\
\showTerm (TmFloat val) = tell $ show val\n\
\showTerm (TmTimesFloat t1 t2) = tell \"(timesfloat \" >>\n\
\                                showTerm t1 >> tell \" \" >>\n\
\                                showTerm t2 >> tell \")\"\n\
\showTerm (TmString str) = tell $ \"\\\"\" ++ str ++ \"\\\"\"\n\
\showTerm (TmSucc t) | isnumericval t = tell $ show $ countSucc 1 t\n\
\                    | otherwise      = showOneArg \"succ\" t\n\
\                    where countSucc c TmZero     = c\n\
\                          countSucc c (TmSucc t) = countSucc (c + 1) t\n\
\showTerm (TmPred t)      = showOneArg \"pred\" t\n\
\showTerm (TmIsZero t)    = showOneArg \"iszero\" t\n\
\showTerm (TmIf t1 t2 t3) = tell \"if \"   >> showTerm t1 >>\n\
\                           tell \"then \" >> showTerm t2 >>\n\
\                           tell \"else \" >> showTerm t3\n\
\showTerm (TmAscribe t ty) = showTerm t >> tell \" as \" >> showType ty\n\
\showTerm (TmBind var binding) = modify (appendBinding var binding) >> \n\
\                                case binding of\n\
\                                  TyAbbBind _ -> tell $ var ++ \" :: *\"\n\
\                                  otherwise   -> tell var\n\
\showTerm (TmVar idx ctxLen) = showVar idx ctxLen\n\
\showTerm (TmAbs var ty body)\n\
\    = do ctx <- get\n\
\         let name = pickFreshName var ctx\n\
\         tell $ \"(lambda \" ++ name ++ \":\"\n\
\         showType ty\n\
\         tell \". \"\n\
\         withBinding name (VarBind ty) $ showTerm body\n\
\         tell \")\"\n\
\showTerm (TmLet var t body)\n\
\    = do ctx <- get\n\
\         let name = pickFreshName var ctx\n\
\         tell $ \"let \" ++ name ++ \" = \"\n\
\         showTerm t\n\
\         tell \" in \"\n\
\         withBinding name NameBind $ showTerm body\n\
\showTerm (TmApp t1 t2) = case t2 of\n\
\                           TmApp _ _ -> showTerm t1 >> tell \" (\" >> \n\
\                                        showTerm t2 >> tell \")\"\n\
\                           otherwise -> showTerm t1 >> tell \" \" >> showTerm t2\n\
\showTerm (TmRecord [])     = tell \"{}\"\n\
\showTerm (TmRecord (f:fs)) = tell \"{\" >> showField f >>\n\
\                             mapM_ (\\f -> tell \", \" >> showField f) fs >>\n\
\                             tell \"}\"\n\
\    where showField (n,t) | isnumber n = showTerm t\n\
\                          | otherwise  = tell (n ++ \"=\") >> showTerm t\n\
\showTerm (TmCase t (c:cs)) = tell \"(case \" >> showTerm t >> tell \" of \" >>\n\
\                             showBranch c >> showBranches cs >> tell \")\"\n\
\    where showBranches []     = return ()\n\
\          showBranches (c:cs) = tell \" | \" >> showBranch c >> showBranches cs\n\
\          showBranch (label, (var, term)) \n\
\              = do tell (\"<\" ++ label ++ \"=\" ++\n\
\                         var ++ \"> ==> \")\n\
\                   withBinding var NameBind $ showTerm term\n\
\showTerm (TmTag var t ty) = tell (\"<\" ++ var ++ \"=\") >>\n\
\                            showTerm t >>\n\
\                            tell \"> as \" >>\n\
\                            showType ty\n\
\showTerm (TmProj t name) = showTerm t >> tell (\".\" ++ name)\n\
\showTerm (TmInert ty) = tell \"inert[\" >> showType ty >> tell \"]\"\n\
\showTerm t = tell $ show t\n\
\\n\
\{- --------------------------------\n\
\   Printing a list of Terms\n\
\   -------------------------------- -}\n\
\\n\
\showTerms :: [Term] -> [Ty] -> ThrowsError String\n\
\showTerms ts = runPrinter . mapM_ showLine . zip ts\n\
\    where showLine (t,ty) = do origCtx <- get\n\
\                               showTerm t\n\
\                               withContext origCtx $ showTypeOfTerm t ty\n\
\                               tell \"\\n\"\n\
\\n\
\-- we need to special handling for binders, but otherwise\n\
\-- we just use the type that was passed in\n\
\showTypeOfTerm :: Term -> Ty -> Printer ()\n\
\showTypeOfTerm (TmBind _ (TyAbbBind _)) _ = return () \n\
\showTypeOfTerm (TmBind var TyVarBind)   _ = return ()\n\
\showTypeOfTerm _ ty                       = tell \" : \" >> showType ty\n\
\\n\
\{- --------------------------------\n\
\   Helpers\n\
\   -------------------------------- -}\n\
\\n\
\showOneArg :: String -> Term -> Printer ()\n\
\showOneArg name t = tell \"(\" >> tell name >> tell \" \" >>\n\
\                    showTerm t >>\n\
\                    tell \")\"\n\
\\n\
\-- Pile on another monad transformer to create a monad that \n\
\-- encapsulates:\n\
\--    (1) accumulating a string\n\
\--    (2) a Context (the state)\n\
\--    (3) the possibility of errors (ThrowsError)\n\
\\n\
\type Printer = WriterT String ContextThrowsError\n\
\\n\
\runPrinter :: Printer () -> ThrowsError String\n\
\runPrinter = runContextThrows . execWriterT \n\
\\n\
\liftThrowsToPrinter :: ThrowsError a -> Printer a\n\
\liftThrowsToPrinter = lift . liftThrows\n\
\\n\
\-- for printing the name of a TyVar or a TmVar\n\
\showVar :: Int -> Int -> Printer ()\n\
\showVar idx ctxLen = do ctx <- get\n\
\                        if ctxLength ctx == ctxLen\n\
\                          then do name <- liftThrowsToPrinter $ nameOf idx ctx\n\
\                                  tell name\n\
\                          else throwError $ Default $ \"Context length mismatch: \" ++ \"var \" ++ (show idx) ++ \" had \" ++ show ctxLen ++ \", but the context length was \" ++ show (ctxLength ctx) ++ \" in the context: \" ++ show ctx\n\
\\n\
\isnumber :: String -> Bool                               \n\
\isnumber n = elem n $ map show [0..9]\n"                      