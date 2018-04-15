{-# Language FlexibleContexts #-}
module Textual (render) where
import Types
import qualified Data.List as L
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T

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
        = defs & target .~ name <> "ID"
               & multiplicity .~ "[1,1]"
               & kind .~ Primary

mergeAll :: S ()
mergeAll = do
    m <- get
    mapM_ (uncurry mergeSingle) (M.toList m)
mergeSingle :: Ident -> [Connection] -> S ()
mergeSingle original conns = do
    let foreignKeys = filter (\s -> s^.kind == Foreign) conns
    let candidate = L.find (\s -> s^.multiplicity == "[1,1]") foreignKeys
    case candidate of
        Just selected -> do
            let others = filter (/=selected) conns & each . kind .~ Owned
            modifying (ix $ selected^.target) (++others)
            modify (M.delete original)
        Nothing -> return ()

renderEntity :: Entity -> S Text
renderEntity e = do
    primIdents <- getPrimaryKeys (e^.connections)
    -- TODO: Extend borrowed (non-primar foreign) keys for merged relationships
    let
        primaries = renderEntry <$> primIdents
        attrs = renderEntry <$> filter (not.isPrimary) (e^.connections)
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
renderEntry :: Connection -> Text
renderEntry con
  | isForeign con = "$\\overline{\\text{" <> t <> "}}$"
  | otherwise = t
  where t = con^.target._Wrapped

    
getPrimaryKeys :: [Connection] -> S [Connection]
getPrimaryKeys conns = do
    ls <- traverse getPrimary conns
    return $ concat ls

getPrimary :: Connection -> S [Connection]
getPrimary con = case con^.kind of
    Primary -> return [con]
    Foreign -> markForeign <$> recurse
    Super -> markForeign <$> recurse
    _ -> return []
  where
    recurse = getPrimaryKeys =<< (getKeys $ con^.target)
    markForeign ls = ls & each . kind .~ Foreign
isPrimary :: Connection -> Bool
isPrimary con = (con^.kind `notElem` [Attr, Owned])
isForeign :: Connection -> Bool
isForeign con = con^.kind `elem` [Super, Foreign, Owned]
    
getKeys :: Ident -> S [Connection]
getKeys name = do
    m <- get
    case m^? ix name of
        Nothing -> error $ name ^._Wrapped.unpacked
        Just ls -> return ls
