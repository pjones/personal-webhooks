-- |
--
-- Copyright:
--   This file is part of the package personal-webhooks. It is subject
--   to the license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/personal-webhooks
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Web.Hooks.Personal.Internal.Logging.Prim
  ( -- * Logging
    MonadLog,
    logCriticalThenDie,
    runLogger,

    -- * Re-exports
    Log.logError,
    Log.logWarning,
    Log.logInfo,
    Log.logDebug,
  )
where

import Control.Exception.Safe (MonadMask)
import Control.Monad.Log (WithSeverity)
import qualified Control.Monad.Log as Log
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc (Doc, pretty)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Time as Time
import Web.Hooks.Personal.Internal.Logging.Config

type MonadLog m = Log.MonadLog (WithSeverity Text) m

logCriticalThenDie ::
  MonadIO m =>
  MonadLog m =>
  Text ->
  m a
logCriticalThenDie msg = do
  Log.logCritical msg
  exitFailure

-- | Discharge the 'MonadLog' effect.
runLogger ::
  MonadIO m =>
  MonadMask m =>
  Config ->
  Log.LoggingT (WithSeverity Text) m a ->
  m a
runLogger config m =
  Log.withFDHandler
    Log.defaultBatchingOptions
    (configHandle config)
    (configRibbonFrac config)
    (configWidth config)
    $ \printer ->
      Log.runLoggingT m (format config >=> maybe pass printer)

-- | Format a log message.
format :: forall m ann. MonadIO m => Config -> Message -> m (Maybe (Doc ann))
format Config {..} msg
  | Log.msgSeverity msg <= configSeverity =
    Just <$> formatMessage configStyle msg
  | otherwise = pure Nothing
  where
    formatTime :: Time.UTCTime -> String
    formatTime =
      Time.formatTime
        Time.defaultTimeLocale
        (Time.iso8601DateFormat (Just "%H:%M:%S"))
    formatMessage :: LoggingStyle -> Message -> m (Doc ann)
    formatMessage = \case
      LogSimple ->
        Log.discardSeverity
          >>> pretty
          >>> pure
      LogSeverity ->
        formatSeverity
          >>> pure
      LogTimestamps ->
        Log.timestamp
          >=> ( Log.renderWithTimestamp
                  formatTime
                  formatSeverity
                  >>> pure
              )
    formatSeverity :: WithSeverity Text -> Doc ann
    formatSeverity (Log.WithSeverity s m) =
      let t = case s of
            Warning -> " WARN"
            Notice -> " NOTE"
            Informational -> " INFO"
            other -> show other & Text.take 5 & Text.toUpper
       in PP.brackets (pretty t) PP.<+> PP.align (pretty m)
