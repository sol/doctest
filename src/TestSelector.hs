module TestSelector 
  (extractTestSelectors
  , filterModuleContent
  , filterModules
  , TestSelector (..)
  , LineSelector (..)
  , Args (..)
  , ArgParserError (..)
  ) where

import           Extract (Module,moduleName,moduleContent) 
import           Location 
                 (Located (Located)
                 , Location (Location,UnhelpfulLocation))  
import           Parse (DocTest)
import           Data.List (isPrefixOf,stripPrefix)
import           Data.Monoid (Monoid (mempty,mappend))
import           Control.Applicative ((<$>),(<*>),pure)
import           Control.Monad.Trans.State 
                 ( StateT (StateT)
                 , evalStateT
                 , runStateT )
import           Data.Char (isDigit,isLetter)
import           Data.Maybe (fromMaybe)
import           Data.Either (rights)

type GhcArg = String
data Args = Args [TestSelector] [GhcArg] deriving (Show,Eq)

instance Monoid Args where 
  mappend (Args ats aghc) (Args bts bghc) = Args (ats ++ bts) (aghc ++ bghc)
  mempty = Args [] []

data TestSelector = TestSelector {
  selectModule :: String 
  , lineSelector :: LineSelector 
  } deriving (Show,Eq)

data LineSelector = 
  AllLines | 
  SingleLine Int | 
  FromStart Int | 
  FromEnd Int | 
  LineRange Int Int 
  deriving (Show,Eq)

data ArgParserError = ArgParserError { 
  expected :: String,
  remainingText :: String
  } deriving (Eq)

instance Show ArgParserError where
  show (ArgParserError e remain) = 
    unwords [
      "Error parsing"
      , prefix 
      , "arg. Expected"
      , e
      , "at '" ++ remain ++ "'"]      

type ArgParserEither = Either ArgParserError
type ArgParser a = StateT String ArgParserEither a

extractTestSelectors :: [String] -> ArgParserEither Args
extractTestSelectors = foldl accumSelector $ Right mempty
  where     
    accumSelector :: ArgParserEither Args -> String -> ArgParserEither Args
    accumSelector a arg = 
      mappend <$> a <*> if prefix `isPrefixOf` arg 
                        then fmap (\ts -> Args [ts] []) $ parseTestSelector arg
                        else pure $ Args [] [arg]
                        
    parseTestSelector :: String -> ArgParserEither TestSelector
    parseTestSelector s = flip evalStateT s $ do
      expectText prefix
      expectText "="
      modNm   <- parseModule
      lineSel <- firstMatch [
        parseLineRange
        , parseFromStart
        , parseFromEnd
        , parseSingleLine
        , parseAllLines
        ]
        "<Empty>|:<LineNum>|:-<EndLine>|:<StartLine>-|:<StartLine>-<EndLine>"
      return $ TestSelector modNm lineSel

    parseAllLines = const AllLines <$> expectEof
    parseLineRange = do
      start <- parseLineStart
      end   <- parseLineEnd
      expectEof
      return $ LineRange start end
      
    parseFromStart = do
      expectText ":"
      end   <- parseLineEnd
      expectEof
      return $ FromStart end

    parseFromEnd = do
      start <- parseLineStart
      expectText "-"
      expectEof
      return $ FromStart start
      
    parseSingleLine = do 
      start <- parseLineStart
      expectEof
      return $ SingleLine start

    parseModule = do
      modStart <- expect isLetter "Module name starting with a letter"
      modRest  <- fromMaybe "" <$> tryParse (spanParse (/= ':') "Module name")
      return (modStart:modRest)

    firstMatch ps desc = StateT $ \s ->
      maybe 
        (Left $ ArgParserError desc s)
        Right 
        ( headMaybe . rights . map (`runStateT` s) $ ps)

    expect :: (Char -> Bool) -> String -> ArgParser Char
    expect p d = StateT $ \s -> 
      maybe 
        (Left $ ArgParserError d s) 
        (\c -> if p c then Right (c,tail s) else Left $ ArgParserError d s)
        (headMaybe s)

    expectEof = StateT $ \s -> 
      if null s then Right ((),s) else Left $ ArgParserError "" s  

    headMaybe []     = Nothing
    headMaybe (x:_) = Just x

    parseLineStart = do
      expectText ":" 
      read <$> spanParse isDigit "Line number start"

    parseLineEnd = do
      expectText "-" 
      read <$> spanParse isDigit "Line number end"

    expectText :: String -> ArgParser () 
    expectText t = StateT $ \s ->
      maybe 
        (Left $ ArgParserError t s) 
        (\rest -> Right ((),rest)) 
        (stripPrefix t s)

    spanParse :: (Char -> Bool) -> String -> ArgParser String
    spanParse f desc = StateT $ \s -> 
      case span f s of
        ([],rest) -> (Left . ArgParserError desc) rest
        t         -> Right t 

    tryParse :: ArgParser a -> ArgParser (Maybe a)
    tryParse p = StateT $ \s -> Right $
      either 
        (const (Nothing,s)) 
        ( \(a,s') -> (Just a , s')) 
        (runStateT p s) 

prefix :: String    
prefix = "--dt-select"

filterModules :: 
  [TestSelector] -> 
  [Module [Located DocTest]] -> 
  [Module [Located DocTest]]
filterModules ss = 
  filter (not . null . moduleContent) . map (filterModuleContent ss)

filterModuleContent :: 
  [TestSelector] -> 
  Module [Located DocTest] -> 
  Module [Located DocTest] 
filterModuleContent [] m = m
filterModuleContent ss m = filterContent applicableSelectors 
  where 
   applicableSelectors = filter ((moduleName m ==) . selectModule ) ss
   filterContent ss' = m { moduleContent = filteredContent ss' }
   
   filteredContent ss' = 
     filter (not . null) $ map (filter $ filterDocTest ss') $ moduleContent m
     
   filterDocTest _ (Located (UnhelpfulLocation _) _) = False
   filterDocTest ss' (Located (Location _ l) _) = any (selectorMatches l) ss'

   selectorMatches _ (TestSelector _ AllLines)        = True
   selectorMatches l (TestSelector _ (SingleLine s))  = l == s 
   selectorMatches l (TestSelector _ (FromStart e))   = l <= e
   selectorMatches l (TestSelector _ (FromEnd s))     = l >= s
   selectorMatches l (TestSelector _ (LineRange s e)) = l >= s && l <= e


