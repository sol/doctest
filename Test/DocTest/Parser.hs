module Test.DocTest.Parser where

import System.IO
import Data.List
import Language.Haskell.Parser hiding (parseModule)
import Language.Haskell.Syntax
import Text.ParserCombinators.Parsec
import Test.DocTest

-- error handling
data Error = Error {
	  location	:: SrcLoc
	, message	:: String
	}

instance Show Error where
	show (Error (SrcLoc filename line column) message) = intercalate ":" [filename, show line, show column, message]

reportError :: Error -> a
reportError e = error (show e)

-- parse functions

parseModule :: FilePath -> IO [DocTest]
parseModule fileName = do
	moduleSource <- readFile fileName

	let moduleName =
		case (parseModuleWithMode (ParseMode fileName) moduleSource) of
			ParseFailed l m								-> reportError (Error l m)
			ParseOk (HsModule _ (Module name) _ _ _)	-> name

	docTests <-
		case (parse manyTestsParser fileName moduleSource) of
			--Left err -> do{ putStr "parse error at " ; print err }
			Left err -> fail (show err)
			Right x  -> return x

	return
		(
			map
			(\(expression, result) -> DocTest fileName moduleName expression result)
			docTests
		)

-- Parser

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

singelTestParser :: Parser (String, String) 
singelTestParser = do
	{
		  manyTill anyChar (try docTestStart)
		; expression	<- tillEndOfLine
		; result		<- commentLines
		; return (expression, (unlines result))
	}

manyTestsParser = (many (try singelTestParser))
