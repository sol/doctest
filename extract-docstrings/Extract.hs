module Extract (extract) where

import Prelude hiding (mod)
import GHC hiding (flags)
import FastString (unpackFS)

import GhcUtil (withGhc)
import Data.Generics


parse :: [String] -- ^ flags
      -> [String] -- ^ files/modules
      -> IO [ParsedModule]
parse flags modules = withGhc flags $ do
  mapM (flip guessTarget Nothing) modules >>= setTargets
  depanal [] False >>= mapM parseModule


extract :: [String] -- ^ flags
        -> [String] -- ^ files/modules
        -> IO [String]
extract flags modules = (map unLoc . concatMap docStringsFromModule) `fmap` parse flags modules


-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedMod m => m -> [Located String]
docStringsFromModule mod = maybeAddHeader $ map (fmap unpackDocString) $ concatMap docStringsFromLHsDecl decls
  where
    source = (unLoc . parsedSource) mod
    decls = hsmodDecls source
    maybeAddHeader = maybe id ((:) . fmap unpackDocString) (hsmodHaddockModHeader source)


-- | Extract all docstrings from given declaration.
docStringsFromLHsDecl :: LHsDecl RdrName -> [LHsDocString]
docStringsFromLHsDecl (L loc decl) = case decl of

  -- top-level documentation
  DocD x -> [L loc (docDeclDoc x)]

  -- ValDs contain error thunks, so we ignore them.  The should not contain any
  -- documentation, anyway.
  ValD _ -> []

  -- documentation on type signatures, etc.
  _ -> listify (const True :: LHsDocString -> Bool) decl


-- | Convert a docstring to a plain string.
unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s
