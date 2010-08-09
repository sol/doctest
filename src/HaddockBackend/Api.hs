module HaddockBackend.Api (
  DocTest(..)
, Interaction(..)
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
    source        :: String -- ^ source file
  , module_       :: String -- ^ originating module
  , interactions  :: [Interaction]
} deriving Show


data Interaction = Interaction {
    expression :: String    -- ^ example expression
  , result     :: [String]  -- ^ expected result
} deriving Show


-- | Extract 'DocTest's
getDocTests :: [String]     -- ^ list of Haddock command-line flags
            -> IO [DocTest] -- ^ extracted 'DocTest's
getDocTests args = do
  interfaces <- createInterfaces' args
  return $ concat $ map docTestsFromInterface interfaces


-- | Get name of the module, that is  associated with given 'Interface'
moduleNameFromInterface :: Interface -> String
moduleNameFromInterface = moduleNameString . moduleName . ifaceMod


-- | Get 'DocTest's from 'Interface'.
docTestsFromInterface :: Interface -> [DocTest]
docTestsFromInterface interface = map docTestFromExamples listOfExamples
  where
    listOfExamples    = examplesFromInterface interface
    moduleName' = moduleNameFromInterface interface
    fileName    = ifaceOrigFilename interface
    docTestFromExamples examples =
      DocExample fileName moduleName' $ map interactionFromExample examples
      where
        interactionFromExample e =
          Interaction (exampleExpression e) (exampleResult e)
