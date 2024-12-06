{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRowK (
    CSubRowK(..)
  , parseSubRowK
  , solveSubRowK
  ) where

import Control.Monad (forM)
import Data.Void

import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownField (KnownField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow (Source(..), Target (..), KnownRowField(..))
import Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow (Fields)
import Data.Record.Anon.Internal.Plugin.TC.TyConSubst

import qualified Data.Record.Anon.Internal.Plugin.TC.Row.KnownRow  as KnownRow
import qualified Data.Record.Anon.Internal.Plugin.TC.Row.ParsedRow as ParsedRow

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Parsed form of @SubRowK@
--
-- > SubRow f f' (r :: [(Symbol, k)]) (r' :: [(Symbol, k)])
data CSubRowK = CSubRowK {
      -- | Fields on the LHS
      subrowParsedLHS :: Fields

      -- | Fields on the RHS
    , subrowParsedRHS :: Fields

    , subrowFType1 :: Type
    , subrowFType2 :: Type

      -- | Left-hand side (@r@)
    , subrowTypeLHS :: Type

      -- | Right-hand side (@r'@)
    , subrowTypeRHS :: Type

      -- | Functor argument kind (@k@)
    , subrowTypeKind1 :: Type
    , subrowTypeKind2 :: Type
    }

{-------------------------------------------------------------------------------
  Outputable
-------------------------------------------------------------------------------}

instance Outputable CSubRowK where
  ppr (CSubRowK parsedLHS parsedRHS fType1 fType2 typeLHS typeRHS typeKind1 typeKind2) = parens $
      text "CSubRow" <+> braces (vcat [
          text "subrowParsedLHS"   <+> text "=" <+> ppr parsedLHS
        , text "subrowParsedRHS"   <+> text "=" <+> ppr parsedRHS
        , text "subrowFType1"    <+> text "=" <+> ppr fType1
        , text "subrowFType2"    <+> text "=" <+> ppr fType2
        , text "subrowTypeLHS"     <+> text "=" <+> ppr typeLHS
        , text "subrowTypeRHS"     <+> text "=" <+> ppr typeRHS
        , text "subrowTypeKind1"    <+> text "=" <+> ppr typeKind1
        , text "subrowTypeKind2"    <+> text "=" <+> ppr typeKind2
        ])

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseSubRowK ::
     TyConSubst
  -> ResolvedNames
  -> Ct
  -> ParseResult Void (GenLocated CtLoc CSubRowK)
parseSubRowK tcs rn@ResolvedNames{..} =
    parseConstraint' clsSubRowK $ \ args ->
      case args of
        [typeKind1, typeKind2, fType1, fType2, typeLHS, typeRHS] -> do
          fieldsLHS <- ParsedRow.parseFields tcs rn typeLHS
          fieldsRHS <- ParsedRow.parseFields tcs rn typeRHS
          return $ CSubRowK {
                subrowParsedLHS = fieldsLHS
              , subrowParsedRHS = fieldsRHS
              , subrowFType1    = fType1
              , subrowFType2    = fType2
              , subrowTypeLHS   = typeLHS
              , subrowTypeRHS   = typeRHS
              , subrowTypeKind1  = typeKind1
              , subrowTypeKind2  = typeKind2
              }
        _ -> pprPanic "parseSubRow: expected 3 arguments" $
               text "args" <+> ppr args

{-------------------------------------------------------------------------------
  Evidence
-------------------------------------------------------------------------------}

evidenceSubRowK ::
     ResolvedNames
  -> CSubRowK
  -> [(Target (KnownField Type), Source (KnownRowField Type))]
  -> TcPluginM 'Solve EvTerm
evidenceSubRowK ResolvedNames{..} CSubRowK{..} fields = do
    return $
      evDataConApp
        (classDataCon clsSubRowK)
        typeArgsEvidence
        [ mkCoreApps (Var idEvidenceSubRowK) $ concat [
              map Type typeArgsEvidence
            , [ mkListExpr intTy $
                  map (mkUncheckedIntExpr . fromIntegral) indices ]
            ]
        ]
  where
    typeArgsEvidence :: [Type]
    typeArgsEvidence = [
          subrowTypeKind1
        , subrowTypeKind2
        , subrowFType1
        , subrowFType2
        , subrowTypeLHS
        , subrowTypeRHS
        ]

    -- Indices into the source array, in the order of the target array
    indices :: [Int]
    indices = map (knownRowFieldIndex . getSource . snd) fields

{-------------------------------------------------------------------------------
  Solver
-------------------------------------------------------------------------------}

solveSubRowK ::
     ResolvedNames
  -> Ct
  -> GenLocated CtLoc CSubRowK
  -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
solveSubRowK rn orig (L loc proj@CSubRowK{..}) =
    case ( ParsedRow.allKnown subrowParsedLHS
         , ParsedRow.allKnown subrowParsedRHS
         ) of
      (Just lhs, Just rhs) ->
        case rhs `KnownRow.isSubRowOf` lhs of
          Right inBoth -> do
            eqs <- forM inBoth $ \(Target r, Source l) ->
                      let o = WantedSuperclassOrigin (mkEqPredRole Representational t1 t2) -- Nothing True
                          t1 = (mkAppTy subrowFType1 (knownRowFieldInfo l))
                          t2 = (mkAppTy subrowFType2 (knownFieldInfo r))
                      in newWanted (updateCtLocOrigin loc o) $
                          mkEqPredRole
                            Representational
                              t1 t2
            ev  <- evidenceSubRowK rn proj inBoth
            return (Just (ev, orig), map mkNonCanonical eqs)
          Left _err ->
            -- TODO: Return a custom error message
            return (Nothing, [])
      _otherwise ->
        return (Nothing, [])
