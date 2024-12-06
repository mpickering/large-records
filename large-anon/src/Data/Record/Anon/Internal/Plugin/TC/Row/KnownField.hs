{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

-- | Information about a field in a record
--
-- Intended for qualified import
--
-- > import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
-- > import qualified Data.Record.Anonymous.Row.Record.KnownField as KnownField
module Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (
    -- * Definition
    KnownField(..)
    -- * Interop with @large-generics@
  , fromString
    -- * Code generation
  , toExpr
  , toType
  ) where

import Data.Record.Anon.Internal.Core.FieldName (FieldName(..))
import qualified Data.Record.Anon.Internal.Core.FieldName as FieldName

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Context-free information about a field in a record
--
-- In other words, we do /not/ know the /index/ of the field here, as that
-- depends the context (the particular record it is part of).
data KnownField a = KnownField {
      knownFieldName :: FieldName
    , knownFieldInfo :: a
    }
  deriving (Functor, Foldable)

{-------------------------------------------------------------------------------
  Interop with @large-generics@
-------------------------------------------------------------------------------}

-- | Construct 'KnownField' from just a string
--
-- NOTE: This involves a hash computation. This is unavoidable as long as
-- @large-generics@ does not precompute those.
fromString :: String -> KnownField ()
fromString name = KnownField {
      knownFieldName = FieldName.fromString name
    , knownFieldInfo = ()
    }

{-------------------------------------------------------------------------------
  Code generation
-------------------------------------------------------------------------------}

-- | Name of the field as a term-level expression
toExpr :: ResolvedNames -> KnownField a -> TcPluginM 'Solve CoreExpr
toExpr ResolvedNames{..} KnownField{knownFieldName = FieldName{..}} = do
    string_expr <- mkStringExpr fieldNameLabel
    platform <- getTargetPlatform
    let int_expr = mkIntExprInt platform fieldNameHash
    return $ mkCoreConApps (tyConSingleDataCon tyConFieldName) [int_expr, string_expr]

-- | Type-level pair @'(n, a)@ or @'(n, f a)@
toType :: Maybe Type -> KnownField Type -> Type
toType mf KnownField{knownFieldName = FieldName{..}, knownFieldInfo} =
    -- mkPromotedPairTy is only introduced in ghc 9.2
    mkTyConApp
      (promotedTupleDataCon Boxed 2)
      [ mkTyConTy typeSymbolKindCon -- kind of first arg
      , liftedTypeKind              -- kind of second arg
      , mkStrLitTy (fsLit fieldNameLabel)
      , case mf of
          Just f  -> f `mkAppTy` knownFieldInfo
          Nothing -> knownFieldInfo
      ]

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable a => Outputable (KnownField a) where
  ppr (KnownField name info) = parens $
          text "KnownField"
      <+> ppr name
      <+> ppr info

