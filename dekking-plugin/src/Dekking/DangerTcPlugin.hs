{-# LANGUAGE DataKinds #-}

module Dekking.DangerTcPlugin (dangerTcPlugin) where

import Data.Maybe
import GHC.Core.Predicate
import GHC.Plugins hiding (TcPlugin)
import GHC.Tc.Types.Constraint
import GHC.TcPlugin.API

dangerTcPlugin :: TcPlugin
dangerTcPlugin =
  TcPlugin
    { tcPluginInit = pure (),
      tcPluginSolve = solve,
      tcPluginRewrite = const emptyUFM,
      tcPluginStop = const $ pure ()
    }

solveIrredEquality :: Ct -> Maybe (EvTerm, Ct)
solveIrredEquality constraint = case classifyPredType $ ctPred constraint of
  EqPred NomEq t1 t2 ->
    Just $ (mkPluginUnivEvTerm "DangerEquality" Nominal t1 t2, constraint)
  _ -> Nothing

solve :: () -> TcPluginSolver
solve () _ wanteds =
  pure $! TcPluginOk solutions []
  where
    solutions = mapMaybe solveIrredEquality wanteds
