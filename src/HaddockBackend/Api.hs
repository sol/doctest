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
  , createInterfaces
  , Flag
  )


data DocTest = DocExample {
    source        :: String -- ^ source file
  , module_       :: String -- ^ originating module
  , interactions  :: [Interaction]
} deriving (Eq, Show, Read)


data Interaction = Interaction {
    expression :: String    -- ^ example expression
  , result     :: [String]  -- ^ expected result
} deriving (Eq, Show, Read)


-- | Extract 'DocTest' from modules and all modules included by the given modules
-- 
-- Note that this function can be called only once during the process lifetime
-- so use it wisely
 
getDocTests :: [Flag]       -- ^ list of Haddock command-line flags
            -> [String]     -- ^ file or module names
            -> IO [DocTest] -- ^ extracted 'DocTest's
getDocTests flags modules = do
  interfaces <- createInterfaces flags modules
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
