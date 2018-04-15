{-# Language FlexibleContexts #-}
module Parser (doParse, parseErrorPretty) where

import Text.Megaparsec as P
import Text.Megaparsec.Text as PT
import qualified Text.Megaparsec.Lexer as L
import Types
import Data.Text (Text)
import Data.Char(isSpace)
import Data.Maybe (fromMaybe, maybeToList)


doParse :: Text -> Either (ParseError Char Dec) [Entity]
doParse input = parse parseFormat "Relationship Format" input

parseFormat :: PT.Parser [Entity]
parseFormat = many parseEntry <* eof
parseEntry :: PT.Parser Entity
parseEntry = (comment >> parseEntry) <|> lexeme parseDef

parseAttribute :: PT.Parser Connection
parseAttribute = (comment >> parseAttribute) <|> parseInner
parseInner :: PT.Parser Connection
parseInner = do
    isForeign <- optional (Foreign <$ char '*')
    name <- parseIdent
    isPrimary <- optional (Primary <$ char '!')
    mult <- try parseMultiplicity
    let conType = fromMaybe Attr $ isForeign <|> isPrimary
    return $ defs
        & kind .~ conType
        & multiplicity .~ mult
        & target .~ name

parseMultiplicity :: Parser Multiplicity
parseMultiplicity = fromString <$> ((char ' ' >> some (satisfy (not.isSpace))) <|> pure "[1,1]")
parseDef :: PT.Parser Entity
parseDef = L.indentBlock sp $ do
    name <-parseIdent
    supertype <- optional $ between (char '(') (char ')') $ parseIdent
    _ <- char ':'
    return (L.IndentSome Nothing (return . mkEntity name supertype) parseAttribute)

mkEntity :: Ident -> (Maybe Ident) -> [Connection] -> Entity
mkEntity name supertype attribs = Entity name (superCon ++ attribs) 
  where
    superCon = superDef <$> maybeToList supertype 
    superDef super = defs
        & kind .~ Super
        & target .~ super
        & multiplicity .~ ""

parseIdent :: IsString a => Parser a
parseIdent = parseText $ oneOf ("_-"::String)
parseText :: IsString a => Parser Char -> Parser a
parseText c = fromString <$> some (c <|> letterChar)

comment :: PT.Parser ()
comment = char '#' >> skipMany (satisfy (/='\n')) >> space
lexeme :: PT.Parser a -> PT.Parser a
lexeme = L.lexeme sp
sp :: PT.Parser ()
sp = space
