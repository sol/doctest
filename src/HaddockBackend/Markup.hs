{-# LANGUAGE CPP #-}
module HaddockBackend.Markup (examplesFromInterface) where

import Name (Name)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Documentation.Haddock (
    markup
  , DocMarkup(..)
#if MIN_VERSION_haddock(2,10,0)
  , Interface(ifaceRnDocMap, ifaceRnArgMap, ifaceRnExportItems, ifaceRnDoc)
#else
  , Interface(ifaceRnDocMap, ifaceRnExportItems, ifaceRnDoc)
  , DocForDecl
  , DocName
#endif
  , Example
  , Doc
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
#if MIN_VERSION_haddock(2,10,0)
    fromDeclarations = extractFromMap (ifaceRnDocMap interface) ++ extractFromArgMap (ifaceRnArgMap interface)

    extractFromArgMap :: Map Name (Map Int (Doc a)) -> [[Example]]
    extractFromArgMap m = map extract $ concatMap (Map.elems) (Map.elems m) -- Map Name (Map Int (Doc a))
#else
    fromDeclarations = fromDeclMap $ ifaceRnDocMap interface

    fromDeclMap :: Map Name (DocForDecl DocName) -> [[Example]]
    fromDeclMap docMap = concatMap docForDeclName $ Map.elems docMap

    docForDeclName :: DocForDecl name -> [[Example]]
    docForDeclName (declDoc, argsDoc)  = argsExamples:declExamples
      where
        declExamples = extractFromMap argsDoc
        argsExamples = maybe [] extract declDoc
#endif

extractFromMap :: Map key (Doc name) -> [[Example]]
extractFromMap m = map extract $ Map.elems m

-- | Extract all 'Example's from given 'Doc' node.
extract :: Doc name -> [Example]
extract = markup exampleMarkup

exampleMarkup :: DocMarkup name [Example]
exampleMarkup = Markup {
  markupEmpty               = mempty
, markupString              = const mempty
, markupParagraph           = id
, markupAppend              = mappend
, markupIdentifier          = const mempty
#if MIN_VERSION_haddock(2,10,0)
, markupIdentifierUnchecked = const mempty
#endif
, markupModule              = const mempty
, markupEmphasis            = id
, markupMonospaced          = id
, markupUnorderedList       = concat
, markupOrderedList         = concat
, markupDefList             = concatMap $ uncurry (++)
, markupCodeBlock           = id
, markupURL                 = const mempty
, markupAName               = const mempty
, markupPic                 = const mempty
, markupExample             = id
}
