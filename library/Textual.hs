{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
module Textual (render) where
import Types
import qualified Data.List as L
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Data.Either(partitionEithers)

toMap :: [Entity] -> M.Map Ident [Connection]
toMap = M.fromList . map (\e -> (e^.ident, e^.connections)) 

render :: [Entity] -> [Text]
render ls = evalState process (toMap ls)

process :: S [Text]
process = do
    addIDs
    -- when shouldMerge mergeAll
    m <- get
    let ls = uncurry Entity <$> M.toList m
    traverse renderEntity ls

type S a = State (M.Map Ident [Connection]) a

addIDs :: S ()
addIDs = imodifying (itraversed <. filtered (none isPrimary)) addID
  where
    addID name conns = artificialKey name : conns
    artificialKey name
        = defs & target .~ "ID"
               & multiplicity .~ One
               & kind .~ Attr
               & primary .~ True

mergeAll :: S ()
mergeAll = do
    m <- get
    mapM_ (uncurry mergeSingle) (M.toList m)
mergeSingle :: Ident -> [Connection] -> S ()
mergeSingle original conns = do
    let foreignKeys = filter (\s -> s^.kind.to isReference) conns
    let candidate = L.find (\s -> s^.multiplicity == One) foreignKeys
    case candidate of
        Just selected -> do
            let others = filter (/=selected) conns & each . kind .~ Owned
            modifying (ix $ selected^.target) (++others)
            modify (M.delete original)
        Nothing -> return ()

renderEntity :: Entity -> S Text
renderEntity e = do
    keys <- concat <$> traverse getDeepKeys (e^.connections)
    -- TODO: Extend borrowed (non-primar foreign) keys for merged relationships
    let
        (prim, other) = partitionEithers keys
        primaries = renderEntry<$> prim 
        attrs = renderEntry <$> other
        primText = wrapPrimaries (combine primaries)
        attrText = combine attrs
        args = combine $ filter (not. T.null) [primText, attrText] 
    return $ e^.ident._Wrapped <> "(" <> args <> ")"
  where
    combine = mconcat . L.intersperse ", "

wrapPrimaries :: Text -> Text
wrapPrimaries t
    | not $ T.null t = "\\underline{" <> t <> "}"
    | otherwise = t
renderEntry :: [Ident] -> Text
renderEntry idents
  | length idents > 1 = wrap base
  | otherwise = base
  where
    base = mconcat $ L.intersperse "." $ map _unNode idents
    wrap :: Text -> Text
    wrap s = "$\\overline{\\text{" <> s <> "}}$"

    

getDeepKeys :: Connection -> S [Either [Ident] [Ident]]
getDeepKeys con0 =
    case con0^. kind of
       Foreign s -> fmap up <$> recurse path0 con0
       Super s -> fmap up <$> recurse path0 con0
       Attr -> return [ up path0 ]
       Owned -> return []
    where
     up path = if con0^.primary then Left path else Right path
     path0 = [con0^.target]
     recurse :: [Ident] -> Connection -> S [[Ident]]
     recurse path con = do
         keys <- getKeys (con^.target)
         deepPrimaries <- traverse (getDeepPrimaries path) keys
         return (concat deepPrimaries)
     getDeepPrimaries path con
         | not (con ^. primary) = return []
         | otherwise = case con^. kind of
             Foreign s -> recurse path' con
             Super s -> recurse path' con
             Attr -> return [path']
             Owned -> return []
         where path' = path ++ [con ^. target]
isPrimary :: Connection -> Bool
isPrimary con = con ^. primary
    
getKeys :: Ident -> S [Connection]
getKeys name = do
    m <- get
    case m^? ix name of
        Nothing -> return []
        Just ls -> return ls
