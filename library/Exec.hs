module Exec where
import qualified Data.Text.IO as T
import Parser
import GraphMLWriter
import System.Environment
import Textual

main :: IO ()
main = do
    [inpath, outpath] <- getArgs
    content <- T.readFile inpath
    let parsed = doParse content
    case parsed of
        Left err -> putStrLn (parseErrorPretty err)
        Right result -> do
            writeGraph (Just outpath) result
            mapM_ T.putStrLn (render result)
