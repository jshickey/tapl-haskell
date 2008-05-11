{- Context used to implement de Bruijn indices, which are described in 
   chapter 6.  A context is simply a list of strings, indexed in reserve
   order (that is, index 0 of a (Context ["x", "y", "z"]) is "x".

 -}
module Context where

newtype Context = Context [String]

newContext :: Context
newContext = Context []

appendBinding :: Context -> String -> Context
appendBinding (Context ns) n = Context (ns ++ [n])

-- Returns a name not already in use in the context, and binds it to the
-- end of the context, return both the name and the new context as a pair.
-- 
-- Returns the suggested name if it is not already in use in the context.
-- Otherwise, it keeps adding quotes to the end of the name until it
-- finds one that isn't in use.
pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx@(Context bindings) name = 
    let freshName = getName name bindings
    in (appendBinding ctx freshName, freshName)
    where getName name [] = name
          getName name (n:ns) | n == name = getName (name ++ "'") bindings
                              | otherwise = getName name ns

getNameByIndex :: Context -> Int -> String
getNameByIndex (Context ns) index = 
    if length ns > index
    then (reverse ns) !! index
    else error $ "Requested index " ++ show index ++ 
             " of Context of length " ++ show (length ns)

ctxLength :: Context -> Int
ctxLength (Context bindings) = length bindings

