{- Contains data structures and printing functions for simplebool
-}
module SimpleBool where

-- TmVar records both its de Bruijn index and the length of the context
-- it was defined in (as described in Chapter 7, this helps expose
-- implementation errors).  
--
-- TmAbs stores the variable name (e.g., "x" in "lambda x. ...")
-- only so that we can try to use that name when printing the 
-- function (it does not affect how the abstraction is evaluated)
data Term = TmVar { index :: Int, contextLength :: Int }
          | TmAbs String Ty Term
          | TmApp Term Term
          | TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmBind String Binding -- not really a term, just a binding a free variable
            deriving (Show, Eq)

isval :: Term -> Bool
isval (TmAbs _ _ _) = True
isval TmTrue = True
isval TmFalse = True
isval _           = False
     
{- ---------------------
       BINDING
 ----------------------- -}
     
data Binding = VarBind Ty
               deriving (Show, Eq)

{- ---------------------
       TYPING
 ----------------------- -}

data Ty = TyBool
        | TyArr Ty Ty -- represents "t1 -> t2"
          deriving (Eq)

instance Show Ty where
    show TyBool = "Bool"
    show (TyArr ty1@(TyArr _ _) ty2) = "(" ++ show ty1 ++ ")" ++ " -> " ++ show ty2
    show (TyArr ty1 ty2) = show ty1 ++ " -> " ++ show ty2

