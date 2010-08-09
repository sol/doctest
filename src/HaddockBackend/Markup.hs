module HaddockBackend.Markup (examplesFromInterface) where

import Name (Name)
import qualified Data.Map as Map
import Data.Map (Map)
import Documentation.Haddock (
    markup
  , DocMarkup(..)
  , Interface(ifaceRnDocMap, ifaceRnExportItems, ifaceRnDoc)
  , Example
  , DocForDecl
  , Doc
  , DocName
  , ExportItem(ExportDoc)
  )

-- | Extract all 'Example's from given 'Interface'.
examplesFromInterface :: Interface -> [[Example]]
examplesFromInterface interface =
  fromModuleHeader ++ fromExportItems ++ fromDeclarations
  where
    fromModuleHeader = case ifaceRnDoc interface of
      Just doc  -> extract doc
      Nothing   -> []
    fromExportItems =
      concatMap extractFromExportItem . ifaceRnExportItems $ interface
      where
        extractFromExportItem (ExportDoc doc) = extract doc
        extractFromExportItem _ = []
    fromDeclarations = fromDeclMap $ ifaceRnDocMap interface

fromDeclMap :: Map Name (DocForDecl DocName) -> [[Example]]
fromDeclMap docMap = concatMap docForDeclName $ Map.elems docMap

docForDeclName :: DocForDecl name -> [[Example]]
docForDeclName (declDoc, argsDoc)  = declExamples ++ argsExamples
  where
    declExamples = extractFromMap argsDoc
    argsExamples = extractFromMaybe declDoc

extractFromMaybe :: Maybe (Doc name) -> [[Example]]
extractFromMaybe (Just doc) = extract doc
extractFromMaybe Nothing    = []

extractFromMap :: Map key (Doc name) -> [[Example]]
extractFromMap m = concatMap extract $ Map.elems m

-- | Extract all 'Example's from given 'Doc' node.
extract :: Doc name -> [[Example]]
extract = markup exampleMarkup
  where
    exampleMarkup :: DocMarkup name [[Example]]
    exampleMarkup = Markup {
      markupEmpty         = [],
      markupString        = const [],
      markupParagraph     = id,
      markupAppend        = (++),
      markupIdentifier    = const [],
      markupModule        = const [],
      markupEmphasis      = id,
      markupMonospaced    = id,
      markupUnorderedList = concat,
      markupOrderedList   = concat,
      markupDefList       = concat . map combineTuple,
      markupCodeBlock     = id,
      markupURL           = const [],
      markupAName         = const [],
      markupPic           = const [],
      markupExample       = return
      }
      where
        combineTuple = uncurry (++)
