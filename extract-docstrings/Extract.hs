module Extract (extract) where

import Prelude hiding (mod)
import GHC.Paths (libdir)
import GHC
import FastString
import DynFlags


parse :: [String] -> IO [ParsedModule]
parse modules = runGhc (Just libdir) $ do
  dynflags  <- getSessionDynFlags
  _ <- setSessionDynFlags (dopt_set dynflags Opt_Haddock) {
          hscTarget = HscNothing
        , ghcMode   = CompManager
        , ghcLink   = NoLink
        }

  mapM (flip guessTarget Nothing) modules >>= setTargets
  modSums <- depanal [] False
  Succeeded <- load LoadAllTargets
  mapM parseModule modSums

extract :: FilePath -> IO [String]
extract file = (map unLoc . concatMap docStringsFromModule) `fmap` parse [file]

docStringsFromModule :: ParsedMod m => m -> [Located String]
docStringsFromModule mod = foldr f [] decls
  where
    f (L loc (DocD x)) xs = L loc (unpackDocString . docDeclDoc $ x) : xs
    f _ xs = xs

    source = (unLoc . parsedSource) mod
    decls = hsmodDecls source

unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s
