{-# LANGUAGE DeriveDataTypeable #-}
module Extract (extract) where

import           Prelude hiding (mod, catch)
import           Control.Exception

import           Control.DeepSeq (deepseq)
import           Data.Generics

import           GHC hiding (flags)
import           FastString (unpackFS)

import           GhcUtil (withGhc)

-- | A wrapper around `GhcException`, to allow a custom `Show` instance.
newtype WrappedGhcException = WrappedGhcException GhcException
  deriving Typeable

instance Show WrappedGhcException where
  show (WrappedGhcException e) = case e of
    Panic s -> unlines [
        ""
      , "GHC panic: " ++ s
      , ""
      , "This is most likely a bug in doctest."
      , ""
      , "Please report it here: https://github.com/sol/doctest-haskell/issues/new"
      ]
    _ -> show e

instance Exception WrappedGhcException


-- | Parse a list of modules.
parse :: [String] -- ^ flags
      -> [String] -- ^ files/modules
      -> IO [ParsedModule]
parse flags modules = withGhc flags $ do
  mapM (flip guessTarget Nothing) modules >>= setTargets
  depanal [] False >>= mapM parseModule


-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -- ^ flags
        -> [String] -- ^ files/modules
        -> IO [String]
extract flags modules = handle (throwIO . WrappedGhcException) $ do
  mods <- parse flags modules
  let docs = (map unLoc . concatMap docStringsFromModule) mods
  docs `deepseq` return docs


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
