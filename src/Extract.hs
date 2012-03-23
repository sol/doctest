{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, DeriveFunctor #-}
module Extract (Module(..), extract) where

import           Prelude hiding (mod, catch)
import           Control.Monad
import           Control.Applicative
import           Control.Exception

import           Control.DeepSeq (deepseq, NFData(rnf))
import           Data.Generics

import           GHC hiding (flags, Module)
import           NameSet (NameSet)
import           Coercion (Coercion)
import           FastString (unpackFS)
import           Digraph (flattenSCCs)

import           GhcUtil (withGhc)

-- | A wrapper around `SomeException`, to allow for a custom `Show` instance.
newtype ExtractError = ExtractError SomeException
  deriving Typeable

instance Show ExtractError where
  show (ExtractError e) =
    unlines [
        "Ouch! Hit an error thunk in GHC's AST while extracting documentation."
      , ""
      , "    " ++ msg
      , ""
      , "This is most likely a bug in doctest."
      , ""
      , "Please report it here: https://github.com/sol/doctest-haskell/issues/new"
      ]
    where
      msg = case fromException e of
        Just (Panic s) -> "GHC panic: " ++ s
        _              -> show e

instance Exception ExtractError

-- | Documentation for a module grouped together with the modules name.
data Module a = Module {
  moduleName    :: String
, moduleContent :: [a]
} deriving (Eq, Functor)

deriving instance Show a => Show (Module a)

instance NFData a => NFData (Module a) where
  rnf (Module name docs) = name `deepseq` docs `deepseq` ()

-- | Parse a list of modules.
parse :: [String] -- ^ flags
      -> [String] -- ^ files/modules
      -> IO [TypecheckedModule]
parse flags modules = withGhc flags $ do
  mapM (flip guessTarget Nothing) modules >>= setTargets
  mods <- depanal [] False
  let sortedMods = flattenSCCs (topSortModuleGraph False mods Nothing)
  reverse <$> mapM (parseModule >=> typecheckModule >=> loadModule) sortedMods

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -- ^ flags
        -> [String] -- ^ files/modules
        -> IO [Module String]
extract flags modules = do
  mods <- parse flags modules
  let docs = map extractFromModule (map tm_parsed_module mods)

  (docs `deepseq` return docs) `catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      Handler (\e -> throw (e :: AsyncException))
    , Handler (throwIO . ExtractError)
    ]

-- | Extract all docstrings from given module and attach the modules name.
extractFromModule :: ParsedModule -> Module String
extractFromModule m = Module name docs
  where
    docs = map unLoc (docStringsFromModule m)
    name = (moduleNameString . GHC.moduleName . ms_mod . pm_mod_summary) m

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedModule -> [Located String]
docStringsFromModule mod = map (fmap unpackDocString) docs
  where
    source   = (unLoc . pm_parsed_source) mod

    -- we use dlist-style concatenation here
    docs     = (maybe id (:) mHeader . maybe id (++) mExports) decls

    -- We process header, exports and declarations separately instead of
    -- traversing the whole source in a generic way, to ensure that we get
    -- everything in source order.
    mHeader  = hsmodHaddockModHeader source
    mExports = f `fmap` hsmodExports source
      where
        f xs = [L loc doc | L loc (IEDoc doc) <- xs]
    decls    = extractDocStrings (hsmodDecls source)


type Selector a = a -> ([LHsDocString], Bool)

-- | Ignore a subtree.
ignore :: Selector a
ignore = const ([], True)

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)

-- | Extract all docstrings from given value.
extractDocStrings :: Data a => a -> [LHsDocString]
extractDocStrings = everythingBut (++) (([], False) `mkQ` fromLHsDecl
  `extQ` fromLDocDecl
  `extQ` fromLHsDocString
  `extQ` (ignore :: Selector NameSet)
  `extQ` (ignore :: Selector PostTcKind)

  -- HsExpr never contains any documentation, but it may contain error thunks.
  --
  -- Problematic are (non comprehensive):
  --
  --  * parallel list comprehensions
  --  * infix operators
  --
  `extQ` (ignore :: Selector (HsExpr RdrName))

  -- undefined before type checking
  `extQ` (ignore :: Selector Coercion)
  )
  where
    fromLHsDecl :: Selector (LHsDecl RdrName)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
      DocD x -> (select . L loc . docDeclDoc) x

      _ -> (extractDocStrings decl, True)

    fromLDocDecl :: Selector LDocDecl
    fromLDocDecl = select . fmap docDeclDoc

    fromLHsDocString :: Selector LHsDocString
    fromLHsDocString = select

-- | Convert a docstring to a plain string.
unpackDocString :: HsDocString -> String
unpackDocString (HsDocString s) = unpackFS s
