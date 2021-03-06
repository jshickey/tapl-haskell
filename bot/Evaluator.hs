{- Small-step evaluator for bot
 -}
module Evaluator ( parseAndEval ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import Syntax
import SimpleContext
import Typing
import Parser
import TaplError
import Printing

{- ---------------------------------
 sub/shift
 --------------------------------- -}

sub i val t = walk 0 t subVar
    where subVar c v@(TmVar idx _) | c + i == idx = shift c val
                                   | otherwise    = v

shift i t = walk 0 t shiftVar
    where shiftVar c (TmVar idx ctxLen) 
              | idx >= c  = TmVar (idx + i) (ctxLen + i)
              | otherwise = TmVar idx (ctxLen + i)

-- helper function abstracting the common functionality of sub/shift
walk c t f = case t of
               TmVar _ _ -> f c t
               TmAbs var ty body -> TmAbs var (walkType c ty f) 
                                    (walk (c + 1) body f)
               TmApp t1 t2 -> TmApp (walk c t1 f) (walk c t2 f)

walkType c ty f = case ty of
                    TyArr ty1 ty2 -> TyArr (walkType c ty1 f)
                                     (walkType c ty2 f)
                    otherwise -> ty
    where walkFields = map (\(n,ty) -> (n, walkType c ty f))

{- ---------------------------------
 eval1 helper functions
 --------------------------------- -}

eval1Cons :: (Term -> Term) -> Term -> ContextThrowsError (Maybe Term)
eval1Cons constructor = (liftM (liftM constructor)) . eval1

apply term body = shift (-1) $ sub 0 (shift 1 term) body

{- ---------------------------------
 eval1, which executes a single "small step".  Use the Monad Transformer
 ContextThrowsError to implicitly pass around the context during the 
 evaluation.  Return a "Maybe Term", with "Nothing" indicating that
 the input term could not be further reduced.
 --------------------------------- -}

eval1 :: Term -> ContextThrowsError (Maybe Term)
eval1 (TmApp t1@(TmAbs _ _ body) t2) 
    | isval t2  = return $ Just $ apply t2 body
    | otherwise = eval1Cons (TmApp t1) t2
eval1 (TmApp t1 t2) | not $ isval t1 = eval1Cons ((flip TmApp) t2) t1
eval1 _ = return Nothing

{- ---------------------------------
 Full evaluation
 --------------------------------- -}

eval :: Term -> ContextThrowsError Term
eval t = do mt' <- eval1 t
            case mt' of
              Nothing -> return t
              Just t' -> eval t'           

evalTerms :: [Term] -> ThrowsError [Term]
evalTerms ts = runContextThrows $ mapM eval ts

parseAndEval :: String -> ThrowsError String
parseAndEval str = do ts     <- parseBot str
                      evaled <- evalTerms ts 
                      typed  <- typeofTerms ts
                      showTerms evaled typed