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
module Web.Hooks.Personal.Internal.Logging.Config
  ( Config (..),
    LoggingStyle (..),
    Message,
    parser,

    -- * Re-exports
    Severity (..),
  )
where

import Control.Monad.Log (Severity (..), WithSeverity)
import qualified Options.Applicative as OA

type Message = WithSeverity Text

-- | Logging configuration.
data Config = Config
  { -- | What additional details are included in the log.
    configStyle :: LoggingStyle,
    -- | Only log messages that exceed or match this severity.
    configSeverity :: Severity,
    -- | The file handle to write messages to.
    configHandle :: Handle,
    -- | The ribbon fraction used by the pretty printer.
    configRibbonFrac :: Double,
    -- | The number of columns of output to produce.
    configWidth :: Int
  }

-- | Command line parser for logging options.
parser :: OA.Parser Config
parser =
  Config
    <$> loggingStyleOption
    <*> severityOption
    <*> handleOption
    <*> pure 0.4
    <*> pure 80

-- | Control what details are logged.
data LoggingStyle
  = -- | Only display log messages.
    LogSimple
  | -- | Basic logging when using something like syslog.
    LogSeverity
  | -- | Logging with timestamps.
    LogTimestamps

-- | Command line option parser for the logging style.
loggingStyleOption :: OA.Parser LoggingStyle
loggingStyleOption =
  asum
    [ OA.flag'
        LogSimple
        ( mconcat
            [ OA.long "log-plain",
              OA.help "Print log messages with no additional details"
            ]
        ),
      OA.flag'
        LogSeverity
        ( mconcat
            [ OA.long "log-severity",
              OA.help "Print log messages with their severity"
            ]
        ),
      OA.flag'
        LogTimestamps
        ( mconcat
            [ OA.long "log-timestamps",
              OA.help "Print log messages with severity and time"
            ]
        ),
      pure LogTimestamps
    ]

-- | Command line parser for severity.
severityOption :: OA.Parser Severity
severityOption =
  asum
    [ OA.flag'
        Critical
        ( mconcat
            [ OA.long "silent",
              OA.help "Disable logging"
            ]
        ),
      OA.flag'
        Debug
        ( mconcat
            [ OA.long "verbose",
              OA.help "Verbose/debug logging"
            ]
        ),
      pure Informational
    ]

-- | Command line option to choose a file handle.
handleOption :: OA.Parser Handle
handleOption =
  asum
    [ OA.flag'
        stderr
        ( mconcat
            [ OA.long "stderr",
              OA.help "Send log messages to stderr instead of stdout"
            ]
        ),
      pure stdout
    ]
