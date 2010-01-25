module GenSyntax (genSyntax) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genSyntax :: Config -> IOThrowsError ()
genSyntax c =
    lift $ writeToFile "Syntax.hs" $ base ++
    beginTerms ++
    (if (useIsorec c) then isorecTerms else "") ++
    endTerms ++
    middle ++
    beginTypes ++
    (if ((useIsorec c) || (useEquirec c)) then recTypes else "") ++
    endTypes ++
    end

base = "{- Defines the terms, types, and bindings used in the fullsimple\n\
\   implemenation, and provides a couple simple helper functions.\n\
\ -}\n\
\\n\
\module Syntax where\n\
\\n\
\import Control.Monad\n\
\import Control.Monad.State\n\
\import Control.Monad.Error\n\
\\n\
\import TaplError    \n\
\\n"

beginTerms = "{- --------------------------------\n\
\   TERMS\n\
\   -------------------------------- -}\n\
\\n\
\data Term = TmTrue\n\
\          | TmFalse\n\
\          | TmIf Term Term Term\n\
\          | TmCase Term [(String, (String, Term))]\n\
\          | TmTag String Term Ty\n\
\          | TmVar Int Int\n\
\          | TmAbs String Ty Term\n\
\          | TmApp Term Term\n\
\          | TmLet String Term Term\n\
\          | TmFix Term\n\
\          | TmString String\n\
\          | TmUnit\n\
\          | TmAscribe Term Ty\n\
\          | TmRecord [(String, Term)]\n\
\          | TmProj Term String\n\
\          | TmFloat Double\n\
\          | TmTimesFloat Term Term\n\
\          | TmZero\n\
\          | TmSucc Term\n\
\          | TmPred Term\n\
\          | TmIsZero Term\n\
\          | TmInert Ty\n\
\          | TmBind String Binding\n"

isorecTerms = "          | TmFold Ty Term\n\
\          | TmUnfold Ty Term \n"

endTerms = "          deriving (Show, Eq)\n"
           
middle = "\n\
\isnumericval :: Term -> Bool\n\
\isnumericval TmZero          = True\n\
\isnumericval (TmSucc t)      = isnumericval t\n\
\isnumericval (TmAscribe t _) = isnumericval t\n\
\isnumericval _               = False\n\
\\n\
\isval :: Term -> Bool\n\
\isval TmTrue             = True\n\
\isval TmFalse            = True\n\
\isval TmUnit             = True\n\
\isval (TmFloat _)        = True\n\
\isval (TmString _)       = True\n\
\isval (TmAbs _ _ _)      = True\n\
\isval (TmAscribe t _)    = isval t\n\
\isval (TmRecord fs)      = and $ map (\\(_,t) -> isval t) fs\n\
\isval (TmTag _ t _)      = isval t\n\
\isval t | isnumericval t = True\n\
\        | otherwise      = False\n\
\\n"

beginTypes = "{- --------------------------------\n\
\   TYPES\n\
\   -------------------------------- -}\n\
\\n\
\data Ty = TyVar Term -- the Term will always be a TmVar (a hack to reuse TmVar code)\n\
\        | TyId String\n\
\        | TyUnit\n\
\        | TyArr Ty Ty -- \"t1 -> t2\"\n\
\        | TyRecord [(String, Ty)]\n\
\        | TyVariant [(String, Ty)]\n\
\        | TyBool\n\
\        | TyString\n\
\        | TyFloat\n\
\        | TyNat\n\
\        | TyTop\n\
\        | TyBot\n"

recTypes = "        | TyRec Ty Ty\n"
               
endTypes = "          deriving (Show, Eq)\n"

end = "\n\
\{- --------------------------------\n\
\   BINDING\n\
\   -------------------------------- -}\n\
\\n\
\data Binding = NameBind\n\
\             | TyVarBind\n\
\             | VarBind Ty\n\
\             | TmAbbBind Term (Maybe Ty)\n\
\             | TyAbbBind Ty\n\
\               deriving (Show, Eq)\n"