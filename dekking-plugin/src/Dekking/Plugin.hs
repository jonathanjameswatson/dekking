module Dekking.Plugin (plugin) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Dekking.Coverable
import Dekking.DangerTcPlugin
import Dekking.SourceAdapter
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Driver.Env
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.LanguageExtensions
import GHC.TcPlugin.API (mkTcPlugin)
import Path
import Path.IO

plugin :: Plugin
plugin =
  defaultPlugin
    { driverPlugin = \_ hscEnv -> do
        -- See [ref:-XImpredicativeTypes]
        let setImpredicativeTypes fs = xopt_set fs ImpredicativeTypes
        -- Turn off safe haskell, because we don't care about it for a coverage report.
        let turnOffSafeHaskell fs = fs {safeHaskell = Sf_Ignore}
        -- Turn off inferring safe haskell, because we don't care about it for a coverage report.
        let turnOffSafeInfer fs = fs {safeInfer = False}
        -- Turn off all warnings, because the resulting source may cause warnings.
        let turnOffWarnings fs = fs {warningFlags = EnumSet.empty, fatalWarningFlags = EnumSet.empty}
        pure
          hscEnv
            { hsc_dflags =
                turnOffWarnings
                  . turnOffSafeInfer
                  . turnOffSafeHaskell
                  . setImpredicativeTypes
                  $ hsc_dflags hscEnv
            },
      parsedResultAction = adaptParseResult,
      tcPlugin = const $ Just $ mkTcPlugin dangerTcPlugin
    }

-- [tag:-XImpredicativeTypes]
--
-- In order to perform the source-to-source transformation, we have to set 'ImpredicativeTypes'.
--
-- For the purposes of this explanation, our sourc-transformation might
-- as well be `a` -> `id a`.
-- One would think (or at least I certainly did), that this would turn
-- any piece of code that type-checksinto something that also
-- type-checks.
-- However, without 'ImpredicativeTypes', it doesn't.
--
-- Indeed, without 'ImpredicativeTypes', this type-checks:
--
-- ```
-- exampleStringL :: Lens' Example String
-- exampleStringL = lens exampleString (\e s -> e {exampleString = s})
-- ```
--
-- But this doesn't:
--
-- ```
-- exampleStringL :: Lens' Example String
-- exampleStringL = (id lens) exampleString (\e s -> e {exampleString = s})
-- ```
--
-- For a simpler example, consider the following piece of code:
-- (Thank you @lnnf107 on twitter!)
--
-- ```
-- f :: Int -> (forall a. a -> a)
-- ```
--
-- Our transformation would turn `f` into `id f`, but then GHC would try
-- to instantiate the type-parameter of `id` with the polytype `Int ->
-- (forall a. a -> a)`, which is only possible with ImpredicativeTypes.

adaptParseResult :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
adaptParseResult es ms pr = do
  liftIO $ putStrLn "Activating the coverage logger plugin"
  let m = ms_mod ms
  let mn = moduleName m
  let exceptionModules = mapMaybe (stripPrefix "--exception=") es
  if "Paths_" `isPrefixOf` moduleNameString mn || moduleNameString mn `elem` exceptionModules
    then pure pr
    else do
      -- Transform the source
      (lm', coverables) <- runReaderT (runWriterT (adaptLocatedHsModule (hpm_module pr))) m
      forM_ (ml_hs_file (ms_location ms)) $ \sourceFile ->
        -- Output the coverables
        liftIO $ do
          p <- resolveFile' sourceFile
          sourceCode <- readFile sourceFile
          coverablesFile <- addExtension coverablesExtension p
          putStrLn $
            unwords
              [ "Outputing coverables file",
                fromAbsFile coverablesFile,
                "for source file",
                fromAbsFile p
              ]
          writeModuleCoverablesFile coverablesFile $
            ModuleCoverablesFile
              { moduleCoverablesFilePackageName = unitToString (moduleUnit m),
                moduleCoverablesFileModuleName = moduleNameString mn,
                moduleCoverablesFileSource = sourceCode,
                moduleCoverablesFileCoverables = coverables
              }
      pure pr {hpm_module = lm'}
