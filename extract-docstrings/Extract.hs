module Extract (extract) where

import Prelude hiding (mod)
import GHC hiding (flags)
import FastString (unpackFS)

import GhcUtil (withGhc)


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

docStringsFromModule :: ParsedMod m => m -> [Located String]
docStringsFromModule mod = maybeAddHeader $ foldr f [] decls
  where
    f (L loc (DocD x)) xs = L loc (unpackDocString . docDeclDoc $ x) : xs
    f _ xs = xs

    source = (unLoc . parsedSource) mod
    decls = hsmodDecls source
    maybeAddHeader = maybe id ((:) . fmap unpackDocString) (hsmodHaddockModHeader source)

unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s
