module GenEvaluator (genEvaluator) where

import Control.Monad.Error

import Util
import TaplError
import Config
        
genEvaluator :: Config -> IOThrowsError ()
genEvaluator c =
    lift $ writeToFile "Evaluator.hs" $ begin ++
             (if (useIsorec c) then isorecEval else "") ++
             (if (useEquirec c) then equirecEval else "") ++
             end

begin = "    \n\
\{- Small-step evaluator for fullsimple \n\
\ -}\n\
\module Evaluator ( parseAndEval ) where\n\
\\n\
\import Control.Monad\n\
\import Control.Monad.State\n\
\import Control.Monad.Error\n\
\\n\
\import Syntax\n\
\import SimpleContext\n\
\import Typing\n\
\import Parser\n\
\import TaplError\n\
\import Printing\n\
\\n\
\{- ---------------------------------\n\
\ sub/shift\n\
\ --------------------------------- -}\n\
\\n\
\sub i val t = walk 0 t subVar\n\
\    where subVar c v@(TmVar idx _) | c + i == idx = shift c val\n\
\                                   | otherwise    = v\n\
\\n\
\shift i t = walk 0 t shiftVar\n\
\    where shiftVar c (TmVar idx ctxLen) \n\
\              | idx >= c  = TmVar (idx + i) (ctxLen + i)\n\
\              | otherwise = TmVar idx (ctxLen + i)\n\
\\n\
\-- helper function abstracting the common functionality of sub/shift\n\
\walk c t f = case t of\n\
\               TmVar _ _ -> f c t\n\
\               TmAbs var ty body -> TmAbs var (walkType c ty f) \n\
\                                    (walk (c + 1) body f)\n\
\               TmLet var t body -> TmLet var (walk c t f) \n\
\                                   (walk (c + 1) body f)\n\
\               TmApp t1 t2 -> TmApp (walk c t1 f) (walk c t2 f)\n\
\               TmSucc t -> TmSucc $ walk c t f\n\
\               TmPred t -> TmPred $ walk c t f\n\
\               TmIsZero t -> TmIsZero $ walk c t f\n\
\               TmIf t1 t2 t3 -> TmIf (walk c t1 f) (walk c t2 f) (walk c t3 f)\n\
\               TmTimesFloat t1 t2 -> TmTimesFloat (walk c t1 f) (walk c t2 f)\n\
\               TmAscribe t ty -> TmAscribe (walk c t f) (walkType c ty f)\n\
\               TmProj t i -> TmProj (walk c t f) i\n\
\               TmRecord fs -> TmRecord $ map (\\(n,t) -> (n, (walk c t f))) fs\n\
\               TmCase t branches -> TmCase (walk c t f) $\n\
\                                    map (\\(n,(v,t)) -> \n\
\                                         (n,(v, (walk (c+1) t f)))) branches\n\
\               TmTag v t ty -> TmTag v (walk c t f) (walkType c ty f)\n\
\               TmInert ty -> TmInert $ walkType c ty f\n\
\               TmFix t -> TmFix $ walk c t f\n\
\               otherwise -> t\n\
\\n\
\walkType c ty f = case ty of\n\
\                    TyVar v -> TyVar $ f c v\n\
\                    TyArr ty1 ty2 -> TyArr (walkType c ty1 f)\n\
\                                     (walkType c ty2 f)\n\
\                    TyRecord  fields -> TyRecord  $ walkFields fields\n\
\                    TyVariant fields -> TyVariant $ walkFields fields\n\
\                    otherwise -> ty\n\
\    where walkFields = map (\\(n,ty) -> (n, walkType c ty f))\n\
\\n\
\{- ---------------------------------\n\
\ eval1 helper functions\n\
\ --------------------------------- -}\n\
\\n\
\eval1Cons :: (Term -> Term) -> Term -> ContextThrowsError (Maybe Term)\n\
\eval1Cons constructor = (liftM (liftM constructor)) . eval1\n\
\\n\
\apply term body = shift (-1) $ sub 0 (shift 1 term) body\n\
\\n\
\{- ---------------------------------\n\
\ eval1, which executes a single \"small step\".  Use the Monad Transformer\n\
\ ContextThrowsError to implicitly pass around the context during the \n\
\ evaluation.  Return a \"Maybe Term\", with \"Nothing\" indicating that\n\
\ the input term could not be further reduced.\n\
\ --------------------------------- -}\n\
\\n\
\eval1 :: Term -> ContextThrowsError (Maybe Term)\n\
\eval1 (TmSucc t)   | isval t   = return Nothing\n\
\                   | otherwise = eval1Cons TmSucc t\n\
\eval1 (TmPred TmZero)          = return $ Just TmZero\n\
\eval1 (TmPred (TmSucc t)) \n\
\    | isnumericval t           = return $ Just t\n\
\eval1 (TmPred t)   | isval t   = return Nothing\n\
\                   | otherwise = eval1Cons TmPred t\n\
\eval1 (TmIsZero TmZero)        = return $ Just TmTrue\n\
\eval1 (TmIsZero t) | isval t   = return $ Just TmFalse\n\
\                   | otherwise = eval1Cons TmIsZero t\n\
\eval1 (TmIf TmTrue  c a)       = return $ Just c\n\
\eval1 (TmIf TmFalse c a)       = return $ Just a\n\
\eval1 (TmIf p c a) | isval p   = return Nothing \n\
\                   | otherwise = eval1Cons (\\p' -> TmIf p' c a) p\n\
\eval1 (TmTimesFloat (TmFloat f1) (TmFloat f2))\n\
\                     = return $ Just $ TmFloat $ f1 * f2\n\
\eval1 (TmTimesFloat t1@(TmFloat _) t2) \n\
\    | not $ isval t2 = eval1Cons (TmTimesFloat t1) t2\n\
\eval1 (TmTimesFloat t1 t2) \n\
\    | not $ isval t1 = eval1Cons ((flip TmTimesFloat) t2) t1\n\
\eval1 (TmAscribe t _) = return $ Just t\n\
\eval1 t@(TmBind var binding) = modify (appendBinding var binding) >>\n\
\                               return Nothing\n\
\eval1 (TmVar idx ctxLen) = do ctx <- get\n\
\                              binding <- liftThrows $ bindingOf idx ctx \n\
\                              case binding of\n\
\                                TmAbbBind val _ -> return $ Just val\n\
\                                otherwise       -> return Nothing\n\
\eval1 (TmApp t1@(TmAbs _ _ body) t2) \n\
\    | isval t2  = return $ Just $ apply t2 body\n\
\    | otherwise = eval1Cons (TmApp t1) t2\n\
\eval1 (TmApp t1 t2) | not $ isval t1 = eval1Cons ((flip TmApp) t2) t1\n\
\eval1 (TmLet var t body) | isval t   = return $ Just $ apply t body\n\
\                         | otherwise = eval1Cons (\\t' -> TmLet var t' body) t\n\
\eval1 (TmRecord fs) = liftM (liftM TmRecord) $ iter fs\n\
\    where iter :: [(String,Term)] -> ContextThrowsError (Maybe [(String,Term)])\n\
\          iter [] = return Nothing\n\
\          iter ((n,t):fs) | isval t   = liftM (liftM ((n,t): )) $ iter fs\n\
\                          | otherwise = liftM (liftM (\\t' -> ((n,t'):fs))) $ eval1 t\n\
\eval1 (TmProj r name) \n\
\    | not $ isval r = eval1Cons (\\r' -> TmProj r' name) r\n\
\eval1 (TmProj r@(TmRecord fs) name) \n\
\    | isval r = access name fs\n\
\    where access name [] = throwError $ EvalError $ \n\
\                           \"Field \" ++ name ++ \" does not exist\"\n\
\          access name ((n,t):fs) | n == name = return $ Just t\n\
\                                 | otherwise = access name fs\n\
\eval1 (TmCase t fs) | not $ isval t = eval1Cons ((flip TmCase) fs) t\n\
\eval1 (TmCase (TmTag var t _) fs) = branch fs\n\
\    where branch [] = throwError $ EvalError \"No applicable branch\"\n\
\          branch ((label, (name, body)):fs)\n\
\              | label == var = return $ Just $ apply t body\n\
\              | otherwise    = return Nothing\n\
\eval1 (TmTag var t ty) | isval t   = return Nothing\n\
\                       | otherwise = eval1Cons (\\t' -> TmTag var t' ty) t\n\
\eval1 (TmFix t) | not $ isval t     = eval1Cons TmFix t\n\
\eval1 t@(TmFix (TmAbs var ty body)) = return $ Just $ apply t body\n"

isorecEval = "eval1 (TmUnfold _ (TmFold _ t)) = eval1 t\n\
\eval1 (TmFold ty t) = eval1Cons (TmFold ty) t\n\
\eval1 (TmUnfold ty t) = eval1Cons (TmUnfold ty) t\n"
 
equirecEval = "" -- todo
                                                                     
end = "eval1 _ = return Nothing\n\
\\n\
\{- ---------------------------------\n\
\ Full evaluation\n\
\ --------------------------------- -}\n\
\\n\
\eval :: Term -> ContextThrowsError Term\n\
\eval t = do mt' <- eval1 t\n\
\            case mt' of\n\
\              Nothing -> return t\n\
\              Just t' -> eval t'           \n\
\\n\
\evalTerms :: [Term] -> ThrowsError [Term]\n\
\evalTerms ts = runContextThrows $ mapM eval ts\n\
\\n\
\parseAndEval :: String -> ThrowsError String\n\
\parseAndEval str = do parsed <- parseFullSimple str\n\
\                      evaled <- evalTerms parsed\n\
\                      typed  <- typeofTerms parsed\n\
\                      showTerms evaled typed\n"