module Test.DocTest.Error where

import Data.List
import Language.Haskell.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

data Error = Error {
	  location	:: SrcLoc
	, message	:: String
	}

instance Show Error where
	show (Error (SrcLoc filename line column) message) = intercalate ":" [filename, show line, show column, " " ++ message]

reportError :: Error -> a
reportError e = error (show e)


_showErrorMessages = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"


toSrcLoc :: SourcePos -> SrcLoc
toSrcLoc x = SrcLoc (sourceName x) (sourceLine x) (sourceColumn x)

toError :: ParseError -> Error
toError e = Error (toSrcLoc (errorPos e)) (_showErrorMessages (errorMessages e))
