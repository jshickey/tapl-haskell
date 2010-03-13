{- Context used to implement de Bruijn indices, which are described in 
   chapter 6.  Each element also includes a Binding, so that we
   can record the type of the variable
 -}
module Context where

import SimpleBool

newtype Context = Context [(String, Binding)]

newContext :: Context
newContext = Context []

appendBinding :: Context -> String -> Binding -> Context
appendBinding (Context ns) n binding = Context (ns ++ [(n,binding)])

-- Returns a name not already in use in the context, and binds it to the
-- end of the context, return both the name and the new context as a pair.
-- 
-- Returns the suggested name if it is not already in use in the context.
-- Otherwise, it keeps adding quotes to the end of the name until it
-- finds one that isn't in use.
pickFreshName :: Context -> String -> Ty -> (Context, String)
pickFreshName ctx@(Context bindings) name ty = 
    let freshName = getName name bindings
    in (appendBinding ctx freshName (VarBind ty), freshName)
    where getName name [] = name
          getName name ((n,_):ns) | n == name = getName (name ++ "'") bindings
                                  | otherwise = getName name ns

getNameByIndex :: Context -> Int -> String
getNameByIndex ctx = fst . (getBindingByIndex ctx)

getBindingByIndex :: Context -> Int -> (String, Binding)
getBindingByIndex (Context ns) index =
    if length ns > index
    then (reverse ns) !! index
    else error $ "Requested index " ++ show index ++ 
             " of Context of length " ++ show (length ns)

ctxLength :: Context -> Int
ctxLength (Context ns) = length ns

showInCtx t@(TmVar _ _) ctx = if contextLength t == ctxLength ctx
                              then getNameByIndex ctx (index t)
                              else error "Context length does match"
showInCtx (TmAbs str ty t) ctx 
    = let (ctx', name) = pickFreshName ctx str ty
      in "(lambda " ++ name ++ ":" ++ show ty ++ ". " ++ 
         showInCtx t ctx' ++ ")"
showInCtx (TmApp t1 t2) ctx = showInCtx t1 ctx ++ " " ++ showInCtx t2 ctx
showInCtx TmTrue ctx = "true"
showInCtx TmFalse ctx = "false"
showInCtx (TmIf p c a) ctx = "if " ++ showInCtx p ctx ++ 
                             " then " ++ showInCtx c ctx ++ 
                             " else " ++ showInCtx a ctx