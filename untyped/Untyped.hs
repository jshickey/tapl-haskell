{- Contains data structures and printing functions for untyped
   lambda calculus.
-}
module Untyped where

import Context

-- TmVar records both its de Bruijn index and the length of the context
-- it was defined in (as described in Chapter 7, this helps expose
-- implementation errors).  
--
-- TmAbs stores the variable name (e.g., "x" in "lambda x. ...")
-- only so that we can try to use that name when printing the 
-- function (it does not affect how the abstraction is evaluated)
data Term = TmVar { index :: Int, contextLength :: Int }
          | TmAbs String Term
          | TmApp Term Term
          | TmBind String -- not really a term, just a binding a free variable
            deriving (Show, Eq)

showInCtx t@(TmVar _ _) ctx = if contextLength t == ctxLength ctx
                              then getNameByIndex ctx (index t)
                              else error "Context length does match"
showInCtx (TmAbs str t) ctx = let (ctx', name) = pickFreshName ctx str
                              in "(lambda " ++ name ++ ". " ++ 
                                 showInCtx t ctx' ++ ")"
showInCtx (TmApp t1 t2) ctx = showInCtx t1 ctx ++ " " ++ showInCtx t2 ctx

showTerms :: [Term] -> Context -> String
showTerms [] _ = ""
-- handle TmBind here, because it is the only that affects
-- terms later in the sequence (as opposed to an abstraction,
-- which affect terms within its subtree)
showTerms ((TmBind str):ts) ctx = str ++ " \n" ++ 
                                  showTerms ts (appendBinding ctx str)
showTerms (t:ts) ctx = showInCtx t ctx ++ "\n" ++ showTerms ts ctx

isval :: Term -> Bool
isval (TmAbs _ _) = True
isval _           = False
     