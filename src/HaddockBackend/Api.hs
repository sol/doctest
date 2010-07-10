module HaddockBackend.Api (
  DocTest(..)
, getDocTests
) where

import Module (moduleName, moduleNameString)
import HaddockBackend.Markup (examplesFromInterface)

import Documentation.Haddock(
    Interface(ifaceMod, ifaceOrigFilename)
  , exampleExpression
  , exampleResult
  , createInterfaces'
  )


data DocTest = DocExample {
    source     :: String    -- ^ source file
  , module_    :: String    -- ^ originating module
  , expression :: String    -- ^ example expression
  , result     :: [String]  -- ^ expected result
} deriving Show


-- | Extract 'DocTest's
getDocTests :: [String]     -- ^ list of Haddock command-line flags
            -> IO [DocTest] -- ^ extracted 'DocTest's
getDocTests args = do
  interfaces <- createInterfaces' args
  return $ concat $ map docTestsFromInterafe interfaces


-- | Get name of the module, that is  associated with given 'Interface'
moduleNameFromInterface :: Interface -> String
moduleNameFromInterface = moduleNameString . moduleName . ifaceMod


-- | Get 'DocTest's from 'Interface'.
docTestsFromInterafe :: Interface -> [DocTest]
docTestsFromInterafe interface = map docTestFromExample examples
  where
    examples    = examplesFromInterface interface
    moduleName' = moduleNameFromInterface interface
    fileName    = ifaceOrigFilename interface
    docTestFromExample e =
      DocExample fileName moduleName' (exampleExpression e) (exampleResult e)
