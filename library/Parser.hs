{-# Language FlexibleContexts #-}
module Parser (doParse, parseErrorPretty) where

import Text.Megaparsec as P
import Text.Megaparsec.Text as PT
import qualified Text.Megaparsec.Lexer as L
import Types
import Data.Text (Text, pack)
import Data.Char(isSpace)
import Data.Maybe (fromMaybe, maybeToList)


doParse :: Text -> Either (ParseError Char Dec) [Entity]
doParse input = parse parseFormat "Relationship Format" input

parseFormat :: PT.Parser [Entity]
parseFormat = many parseEntry <* eof
parseEntry :: PT.Parser Entity
parseEntry = (comment >> parseEntry) <|> lexeme parseDef

parseAttribute :: Ident -> PT.Parser Connection
parseAttribute owner = (comment >> parseAttribute owner) <|> parseInner owner
parseInner :: Ident -> PT.Parser Connection
parseInner owner = do
    isForeign <- optional (Foreign owner <$ char '*')
    name <- parseIdent
    isPrimary <- optional (True <$ char '!')
    mult <- try parseMultiplicity
    let conType = fromMaybe Attr $ isForeign
        primType = fromMaybe False isPrimary

    return $ defs
        & kind .~ conType
        & primary .~ primType
        & multiplicity .~ mult
        & target .~ name

parseMultiplicity :: Parser Multiplicity
parseMultiplicity = do 
    mult <- optional $ char ' ' >> some (satisfy (not.isSpace))
    return $ case mult of
        Just "[1,1]" -> One
        Nothing -> One
        Just "[0,1]" -> Optional
        Just s -> Other (pack s)

parseDef :: PT.Parser Entity
parseDef = L.indentBlock sp $ do
    name <-parseIdent
    supertype <- optional $ between (char '(') (char ')') $ parseIdent
    _ <- char ':'
    return (L.IndentMany Nothing (return . mkEntity name supertype) (parseAttribute name))

mkEntity :: Ident -> (Maybe Ident) -> [Connection] -> Entity
mkEntity name supertype attribs = Entity name (superCon ++ attribs) 
  where
    superCon = superDef <$> maybeToList supertype 
    superDef super = defs
        & kind .~ Super super
        & target .~ super
        & multiplicity .~ One
        & primary .~ True

parseIdent :: IsString a => Parser a
parseIdent = fmap fromString $ some $ satisfy (\c -> not (isSpace c) && c `notElem` ("():!" :: String))

comment :: PT.Parser ()
comment = char '#' >> skipMany (satisfy (/='\n')) >> space
lexeme :: PT.Parser a -> PT.Parser a
lexeme = L.lexeme sp
sp :: PT.Parser ()
sp = space
