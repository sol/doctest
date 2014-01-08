module TestSelector 
  (extractTestSelectors
  , filterModuleContent
  , filterModules
  , TestSelector (..)
  , Args (..)
  , ArgParserError (..)
  ) where

import           Extract (Module,moduleName,moduleContent) 
import           Location 
                 (Located (Located)
                 , Location (Location,UnhelpfulLocation))  
import           Parse (DocTest)
import           Data.List (partition,isPrefixOf,stripPrefix,filter,null)
import           Data.Monoid (Monoid (mempty,mappend))
import           Control.Applicative ((<$>),(<*>),pure)
import           Data.Either (Either,either)
import           Control.Monad.Trans.State 
                 ( StateT (StateT)
                 , get
                 , modify
                 , evalStateT
                 , runStateT )
import           Data.Tuple (swap)
import           Data.Maybe (maybe)
import           Data.Char (isDigit)

type GhcArg = String
data Args = Args [TestSelector] [GhcArg] deriving (Show,Eq)

instance Monoid Args where 
  mappend (Args ats aghc) (Args bts bghc) = Args (ats ++ bts) (aghc ++ bghc)
  mempty = Args [] []

data TestSelector = TestSelector {
  selectModule :: String 
  , lineStart :: Int 
  , lineEnd :: Maybe Int
  } deriving (Show,Eq)

data ArgParserError = ArgParserError { 
  expected :: String,
  remainingText :: String
  } deriving (Eq)

instance Show ArgParserError where
  show (ArgParserError e remain) = 
    "Error parsing " ++ prefix ++ " arg. Expected " ++ e ++ " at " ++ remain

type ArgParserEither = Either ArgParserError
type ArgParser a = StateT String ArgParserEither a

extractTestSelectors :: [String] -> ArgParserEither Args
extractTestSelectors = foldl accumSelector $ Right mempty
  where     
    accumSelector :: ArgParserEither Args -> String -> ArgParserEither Args
    accumSelector a arg = 
      mappend <$> a <*> if isPrefixOf prefix arg 
                        then fmap (\ts -> Args [ts] []) $ parseTestSelector arg
                        else pure $ Args [] [arg]
    parseTestSelector :: String -> ArgParserEither TestSelector
    parseTestSelector s = (flip evalStateT) s $ do
      expectText prefix
      modName  <- spanParse (/= ':') "Module name"
      parseDrop 1 
      ls <- read <$> spanParse isDigit "Line number"
      le <- tryParse parseLineEnd
      return $ TestSelector modName ls le

    parseLineEnd = do
      expectText "-" 
      read <$> spanParse isDigit "Line number"

    parseDrop n = modify (drop n)

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
        (\(a,s') -> (Just a , s')) 
        (runStateT p s)

prefix :: String    
prefix = "--dt-select="

filterModules :: [TestSelector] -> [Module [Located DocTest]] -> [Module [Located DocTest]]
filterModules ss = filter (not . null . moduleContent) . map (filterModuleContent ss)

filterModuleContent :: [TestSelector] -> Module [Located DocTest] -> Module [Located DocTest] 
filterModuleContent [] m = m
filterModuleContent ss m = filterLines $ filter ((moduleName m ==) . selectModule ) ss
  where 
    filterLines ss' = m { moduleContent = filter (not . null) $ map (filter $ filterDocTest ss') $ moduleContent m }
    filterDocTest :: [TestSelector] -> Located DocTest -> Bool
    filterDocTest _ (Located (UnhelpfulLocation _) _) = False
    filterDocTest ss' (Located (Location _ line) _) = 
      any (\s -> line == lineStart s ) ss'

