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
examplesFromInterface interface = filter (not . null) $ [fromModuleHeader] ++ fromExportItems ++ fromDeclarations
  where
    fromModuleHeader = case ifaceRnDoc interface of
      Just doc  -> extract doc
      Nothing   -> []
    fromExportItems =
      map extractFromExportItem . ifaceRnExportItems $ interface
      where
        extractFromExportItem (ExportDoc doc) = extract doc
        extractFromExportItem _ = []
    fromDeclarations = fromDeclMap $ ifaceRnDocMap interface

fromDeclMap :: Map Name (DocForDecl DocName) -> [[Example]]
fromDeclMap docMap = concatMap docForDeclName $ Map.elems docMap

docForDeclName :: DocForDecl name -> [[Example]]
docForDeclName (declDoc, argsDoc)  = argsExamples:declExamples
  where
    declExamples = extractFromMap argsDoc
    argsExamples = maybe [] extract declDoc

extractFromMap :: Map key (Doc name) -> [[Example]]
extractFromMap m = map extract $ Map.elems m

-- | Extract all 'Example's from given 'Doc' node.
extract :: Doc name -> [Example]
extract = markup exampleMarkup
  where
    exampleMarkup :: DocMarkup name [Example]
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
      markupDefList       = concatMap combineTuple,
      markupCodeBlock     = id,
      markupURL           = const [],
      markupAName         = const [],
      markupPic           = const [],
      markupExample       = id
      }
      where
        combineTuple = uncurry (++)
