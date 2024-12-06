{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Record.Anon.Internal.Plugin.TC.Solver (
    solve
  ) where

import Data.Bifunctor
import Data.Maybe (catMaybes)
import Data.Traversable (forM)

import Data.Record.Anon.Internal.Plugin.TC.Constraints.AllFields
import Data.Record.Anon.Internal.Plugin.TC.Constraints.KnownFields
import Data.Record.Anon.Internal.Plugin.TC.Constraints.KnownHash
import Data.Record.Anon.Internal.Plugin.TC.Constraints.RowHasField
import Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRow
import Data.Record.Anon.Internal.Plugin.TC.Constraints.SubRowK
import Data.Record.Anon.Internal.Plugin.TC.GhcTcPluginAPI
import Data.Record.Anon.Internal.Plugin.TC.NameResolution
import Data.Record.Anon.Internal.Plugin.TC.Parsing
import Data.Record.Anon.Internal.Plugin.TC.TyConSubst

{-------------------------------------------------------------------------------
  Top-level solver
-------------------------------------------------------------------------------}

solve :: ResolvedNames -> TcPluginSolver
solve rn@ResolvedNames{..} given wanted = do
--  trace _debugInput  $
--  trace _debugParsed $
    do (solved, new) <- fmap (bimap catMaybes concat . unzip) $ (mapM p wanted)
       return $ TcPluginOk solved new
  where
    tcs :: TyConSubst
    tcs = mkTyConSubst given

    successOrNone res k =
      case res of
       ParseOk a -> k a
       ParseNoMatch -> return (Nothing, [])



    p :: Ct -> TcPluginM 'Solve (Maybe (EvTerm, Ct), [Ct])
    p ct =
           case classifyPredType (ctPred ct) of
            ClassPred cls _ | cls == clsAllFields -> successOrNone (parseAllFields tcs rn ct) (solveAllFields rn ct)
                            | cls == clsKnownFields -> successOrNone (parseKnownFields tcs rn ct) (solveKnownFields rn ct)
                            | cls == clsKnownHash -> successOrNone (parseKnownHash tcs rn ct) (solveKnownHash rn ct)
                            | cls == clsRowHasField -> successOrNone (parseRowHasField tcs rn ct) (solveRowHasField rn ct)
                            | cls == clsSubRow      -> successOrNone (parseSubRow tcs rn ct) (solveSubRow rn ct)
                            | cls == clsSubRowK      -> successOrNone (parseSubRowK tcs rn ct) (solveSubRowK rn ct)
            _ -> return (Nothing, [])


    parsedAllFields   :: [(Ct, GenLocated CtLoc CAllFields)]
    parsedKnownFields :: [(Ct, GenLocated CtLoc CKnownFields)]
    parsedKnownHash   :: [(Ct, GenLocated CtLoc CKnownHash)]
    parsedRowHasField :: [(Ct, GenLocated CtLoc CRowHasField)]
    parsedSubRow      :: [(Ct, GenLocated CtLoc CSubRow)]

    parsedAllFields    = parseAll' (withOrig (parseAllFields   tcs rn)) wanted
    parsedKnownFields  = parseAll' (withOrig (parseKnownFields tcs rn)) wanted
    parsedKnownHash    = parseAll' (withOrig (parseKnownHash   tcs rn)) wanted
    parsedRowHasField  = parseAll' (withOrig (parseRowHasField tcs rn)) wanted
    parsedSubRow       = parseAll' (withOrig (parseSubRow      tcs rn)) wanted
    parsedSubRowK      = parseAll' (withOrig (parseSubRowK      tcs rn)) wanted

    _debugInput :: String
    _debugInput = unlines [
          "*** input"
        , concat [
              "given:"
            , showSDocUnsafe (ppr given)
            ]
        , concat [
              "wanted: "
            , showSDocUnsafe (ppr wanted)
            ]
        ]

    _debugParsed :: String
    _debugParsed = unlines [
          "*** parsed"
        , concat ["parsedAllFields:   ", showSDocUnsafe $ ppr (parsedAllFields )]
        , concat ["parsedKnownFields: ", showSDocUnsafe $ ppr (parsedKnownFields )]
        , concat ["parsedKnownHash:   ", showSDocUnsafe $ ppr (parsedKnownFields )]
        , concat ["parsedRowHasField: ", showSDocUnsafe $ ppr (parsedRowHasField )]
        , concat ["parsedSubRow:      ", showSDocUnsafe $ ppr (parsedSubRow )]
        , concat ["tcs (TyConSubst):  ", showSDocUnsafe $ ppr tcs]
        ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

concatM :: Applicative m => [m [a]] -> m [a]
concatM = fmap concat . sequenceA
