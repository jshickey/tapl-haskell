{- A typed store (heap).  Maps locations to (type, term) pairs.
   Types are stored to make typing tractable, as discussed in Chapter 12.
 -}
module Store where

import Control.Monad
import Control.Monad.Error
import Control.Monad.State

import Syntax
import SimpleContext
import TaplError

newtype Store = MkStore [(Ty, Term)]
    deriving (Show)

newStore :: Store
newStore = MkStore []

storeRef :: Store -> Term -> Ty -> (Term, Store)
storeRef (MkStore cs) t ty = let idx = length cs
                             in (TmLoc idx, MkStore (cs ++ [(ty,t)]))

derefTerm :: Store -> Int -> ThrowsError Term
derefTerm store idx = liftM snd $ derefCell store idx

derefType :: Store -> Int -> ThrowsError Ty
derefType store idx = liftM fst $ derefCell store idx

derefCell :: Store -> Int -> ThrowsError (Ty, Term)
derefCell (MkStore cs) idx
    = if idx < 0 || idx >= length cs
      then throwError $ EvalError $ "Bad index to store: " ++ show idx
      else return $ cs !! idx

assignRef :: Store -> Int -> Term -> ThrowsError Store
assignRef s@(MkStore cs) idx t
    = do ty <- derefType s idx
         return $ MkStore $ take idx cs ++ [(ty,t)] ++ drop (idx + 1) cs

-- To hide both a Context and a Store, we slap another StateT 
-- monad transformer on top of ContextThrows
type StoreContextThrows = StateT Store ContextThrowsError

runStoreContextThrows :: StoreContextThrows a -> ThrowsError a
runStoreContextThrows = runContextThrows . (flip evalStateT) newStore

liftThrowsToStore :: ThrowsError a -> StoreContextThrows a
liftThrowsToStore = lift . liftThrows

withBindingUnderStore var b action = do ctx <- lift get
                                        lift $ put $ appendBinding var b ctx
                                        result <- action
                                        lift $ put ctx
                                        return result