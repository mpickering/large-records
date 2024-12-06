{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Data.Record.Anon.Internal.Plugin.TC.NameResolution (
    ResolvedNames(..)
  , nameResolution
  ) where

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import GHC.TypeError
import GHC.Plugins (thNameToGhcNameIO, HscEnv(..), liftIO)
import GHC.Tc.Plugin (getTopEnv)
import GHC.TcPlugin.API.Internal
import GHC.Data.Maybe

-- | Names we need to parse constraints or generate core
--
-- Listed alphabetically.
data ResolvedNames = ResolvedNames {
      clsAllFields          :: Class
    , clsKnownFields        :: Class
    , clsKnownHash          :: Class
    , clsRowHasField        :: Class
    , clsSubRow             :: Class
    , clsSubRowK            :: Class
    , clsUnsatisfiable      :: Class
    , dataConDictAny        :: DataCon
    , idEvidenceAllFields   :: Id
    , idEvidenceKnownFields :: Id
    , idEvidenceKnownHash   :: Id
    , idEvidenceRowHasField :: Id
    , idEvidenceSubRow      :: Id
    , idEvidenceSubRowK     :: Id
    , idMkDictAny           :: Id
    , tyConDictAny          :: TyCon
    , tyConMerge            :: TyCon
    , tyConFieldTypes       :: TyCon
    , tyConFieldName        :: TyCon
    , tyConPair             :: TyCon
    , tyConSimpleFieldTypes :: TyCon
    , tyConErrorText        :: TyCon
    }

nameResolution :: TcPluginM 'Init ResolvedNames
nameResolution = do
    modl <- getModule "large-anon" "Data.Record.Anon.Plugin.Internal.Runtime"
    modl_fn <- getModule "large-anon" "Data.Record.Anon.Internal.Core.FieldName"


    let getClass       :: MonadTcPlugin m => String -> m Class
        getTyCon       :: MonadTcPlugin m => String -> m TyCon
        getDataCon     :: MonadTcPlugin m => String -> m DataCon
        getVar         :: MonadTcPlugin m => String -> m Id
        getPromDataCon :: MonadTcPlugin m => String -> m TyCon

        getClass       cls = lookupOrig modl (mkTcOcc cls)   >>= tcLookupClass
        getTyCon       con = lookupOrig modl (mkTcOcc con)   >>= tcLookupTyCon
        getDataCon     con = lookupOrig modl (mkDataOcc con) >>= tcLookupDataCon
        getVar         var = lookupOrig modl (mkVarOcc var)  >>= tcLookupId
        getPromDataCon con = promoteDataCon <$> getDataCon con

    clsAllFields          <- getClass "AllFields"
    clsKnownFields        <- getClass "KnownFields"
    clsKnownHash          <- getClass "KnownHash"
    clsRowHasField        <- getClass "RowHasField"
    clsSubRow             <- getClass "SubRow"
    clsSubRowK            <- getClass "SubRowK"

    hsc_env <- liftTcPluginM getTopEnv
    let unsat = ''Unsatisfiable
        text  = 'GHC.TypeError.Text
    let get_ghc_name name = unsafeLiftTcM (liftIO (thNameToGhcNameIO (hsc_NC hsc_env) name))

    clsUnsatisfiable <- tcLookupClass . expectJust "ab" =<< get_ghc_name unsat

    tyConErrorText <- fmap promoteDataCon . tcLookupDataCon . expectJust "ab" =<< get_ghc_name text



    dataConDictAny        <- getDataCon "DictAny"

    idEvidenceAllFields   <- getVar "evidenceAllFields"
    idEvidenceKnownFields <- getVar "evidenceKnownFields"
    idEvidenceKnownHash   <- getVar "evidenceKnownHash"
    idEvidenceRowHasField <- getVar "evidenceRowHasField"
    idEvidenceSubRow      <- getVar "evidenceSubRow"
    idEvidenceSubRowK     <- getVar "evidenceSubRowK"
    idMkDictAny           <- getVar "mkDictAny"

    tyConDictAny          <- getTyCon       "DictAny"
    tyConFieldTypes       <- getTyCon       "FieldTypes"
    tyConMerge            <- getTyCon       "Merge"
    tyConFieldName        <- lookupOrig modl_fn (mkTcOcc "FieldName") >>= tcLookupTyCon
    tyConPair             <- getPromDataCon ":="
    tyConSimpleFieldTypes <- getTyCon       "SimpleFieldTypes"

    return $ ResolvedNames {..}
