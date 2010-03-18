module Test.DocTest.Parser (parseModule) where

import System.IO
import Language.Haskell.Parser hiding (parseModule)
import Language.Haskell.Syntax
import Text.ParserCombinators.Parsec
import Test.DocTest
import Test.DocTest.Error
import Data.List

parseModule :: FilePath -> IO [DocTest]
parseModule fileName = do
	moduleSource <- readFile fileName

	let moduleName =
		case (parseModuleWithMode (ParseMode fileName) moduleSource) of
			ParseFailed l m								-> reportError (Error l m)
			ParseOk (HsModule _ (Module name) _ _ _)	-> name

	docTests <-
		case (parse manyTestsParser fileName moduleSource) of
			--Left  e -> error ("parse error at " ++ show e)
			Left  e	-> reportError (toError e)
			Right x	-> return x

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
		; return (expression, (intercalate "\n" result))
	}

manyTestsParser = (many (try singelTestParser))
