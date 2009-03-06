module Main where

import System.IO
import Data.List
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Text.ParserCombinators.Parsec
import Doctest

data Error = Error {
	  location	:: SrcLoc
	, message	:: String
	}

instance Show Error where
	show (Error (SrcLoc filename line column) message) = intercalate ":" [filename, show line, show column, message]

reportError :: Error -> a
reportError e = error (show e)

run :: Parser a -> String -> IO a
run p input
        = case (parse p "" input) of
			--Left err -> do{ putStr "parse error at " ; print err }
			Left err -> fail (show err)
			Right x  -> return x

tillEndOfLine :: Parser String
tillEndOfLine = manyTill anyChar newline

commentLine :: Parser String
commentLine = do
	{
		  string "-- "
		; tillEndOfLine
	}

commentLines :: Parser [String]
commentLines = many1 commentLine

docTestStart = string "-- > "

docParse :: Parser (String, String) 
docParse = do
	{
		  manyTill anyChar (try docTestStart)
		; expression	<- tillEndOfLine
		; result		<- commentLines
		; return (expression, (unlines result))
	}

moduleParser = (many (try docParse))

myparse :: FilePath -> IO [DocTest]
myparse file = do
	m <- readFile file
	let n = moduleName file m
	l <- run moduleParser m
	return (map (tupleToDocTest file n) l)
	where
		tupleToDocTest file _module (expression, result) = DocTest file _module expression result

moduleName file m =
	case (parseModuleWithMode (ParseMode file) m) of
		ParseFailed srcLoc msg				-> reportError (Error srcLoc msg)
		ParseOk (HsModule _ (Module name) _ _ _)	-> name
