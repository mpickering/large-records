{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Thin layer around ghc-tcplugin-api
module Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI (
    -- * Standard exports
    module GHC.TcPlugin.API
  , module GHC.Builtin.Names
  , module GHC.Builtin.Types
  , module GHC.Builtin.Types.Prim
  , module GHC.Core.Make
  , module GHC.Utils.Outputable
  , getTargetPlatform
  , tyConSingleDataCon
  , CtOrigin(..)
  , GHC.updateCtLocOrigin
  , GHC.setCtLocOrigin

    -- * New functonality
  , isCanonicalVarEq
  , getModule
  , pprString
  ) where

import GHC.Stack
import GHC.Core.TyCon
import qualified GHC.Tc.Plugin as GHC
import qualified GHC.Platform as GHC
import GHC.TcPlugin.API.Internal
import qualified GHC.Tc.Types.Origin
import qualified GHC.Tc.Types.CtLoc as GHC


#if __GLASGOW_HASKELL__ < 900
import Data.List.NonEmpty (NonEmpty, toList)
#endif

import GHC.TcPlugin.API
import GHC.Builtin.Names
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.Make
import GHC.Utils.Outputable

#if __GLASGOW_HASKELL__ >= 810 &&  __GLASGOW_HASKELL__ < 900
import Constraint (Ct(..))
#endif

#if __GLASGOW_HASKELL__ >= 900 &&  __GLASGOW_HASKELL__ < 902
import GHC.Tc.Types.Constraint (Ct(..))
#endif

#if __GLASGOW_HASKELL__ >= 902
import GHC.Tc.Types.Constraint (Ct(..), CanEqLHS(..))
#endif

#if __GLASGOW_HASKELL__ >= 911
import GHC.Tc.Types.Constraint
#endif

getTargetPlatform :: TcPluginM Solve GHC.Platform
getTargetPlatform = liftTcPluginM GHC.getTargetPlatform

isCanonicalVarEq :: Ct -> Maybe (TcTyVar, Type)
#if __GLASGOW_HASKELL__ >= 810 &&  __GLASGOW_HASKELL__ < 902
isCanonicalVarEq = \case
    CTyEqCan{..}  -> Just (cc_tyvar, cc_rhs)
    CFunEqCan{..} -> Just (cc_fsk, mkTyConApp cc_fun cc_tyargs)
    _otherwise    -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 902 && __GLASGOW_HASKELL__ < 911
isCanonicalVarEq = \case
    CEqCan{..}
      | TyVarLHS var <- cc_lhs
      -> Just (var, cc_rhs)
      | TyFamLHS tyCon args <- cc_lhs
      , Just var            <- getTyVar_maybe cc_rhs
      -> Just (var, mkTyConApp tyCon args)
    _otherwise
      -> Nothing
#endif
#if __GLASGOW_HASKELL__ >= 911
isCanonicalVarEq = \case
    CEqCan (EqCt{..})
      | TyVarLHS var <- eq_lhs
      -> Just (var, eq_rhs)
      | TyFamLHS tyCon args <- eq_lhs
      , Just var            <- getTyVar_maybe eq_rhs
      -> Just (var, mkTyConApp tyCon args)
    _otherwise
      -> Nothing
#endif

-- TODO: Ideally we would actually show the location information obviously
instance Outputable CtLoc where
  ppr _ = text "<CtLoc>"

#if __GLASGOW_HASKELL__ < 900
instance Outputable a => Outputable (NonEmpty a) where
  ppr = ppr . toList
#endif

#if __GLASGOW_HASKELL__ >= 902
instance (Outputable l, Outputable e) => Outputable (GenLocated l e) where
  ppr (L l e) = parens $ text "L" <+> ppr l <+> ppr e
#endif

getModule :: (HasCallStack, MonadTcPlugin m) => String -> String -> m Module
getModule pkg modl = do
    let modl' = mkModuleName modl
    pkg' <- resolveImport modl' (Just $ fsLit pkg)
    res  <- findImportedModule modl' pkg'
    case res of
      Found _ m  -> return m
      _otherwise -> error $ concat [
          "getModule: could not find "
        , modl
        , " in package "
        , pkg
        ]

pprString :: String -> SDoc
pprString = text

