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
module Main (main) where

import Options.Applicative
import qualified UI.Create as Create
import qualified UI.List as List
import qualified UI.Run as Run
import qualified UI.Server as Server
import qualified Web.Hooks.Personal.Env as Env
import qualified Web.Hooks.Personal.Internal.Logging as L

-- | Subcommand.
data Command
  = -- | Create a new hook.
    Create !Create.Options
  | -- | List hooks.
    List !List.Options
  | -- | Run hooks.
    Run !Run.Options
  | -- | Web server.
    Server !Server.Options

-- | Command line options
data Options = Options
  { -- | Alternate configuration file to load.
    optionConfigFile :: !(Maybe FilePath),
    -- | Logging configuration.
    optionLogging :: !L.Config,
    -- | Which subcommand to run.
    optionCommand :: !Command
  }

-- | Command line option parser.
parser :: Parser Options
parser =
  Options
    <$> optional
      ( strOption
          ( mconcat
              [ long "config",
                short 'c',
                metavar "FILE",
                help "Load FILE as an alternate config file"
              ]
          )
      )
    <*> L.parser
    <*> subparser
      ( mconcat
          [ createCommand,
            listCommand,
            runCommand,
            serverCommand
          ]
      )
  where
    mkCmd :: String -> String -> Parser a -> Mod CommandFields a
    mkCmd name desc p =
      command name (info (p <**> helper) (progDesc desc))

    createCommand =
      mkCmd "create" "Create a new webhook" (Create <$> Create.parser)

    listCommand =
      mkCmd "list" "List hooks" (List <$> List.parser)

    runCommand =
      mkCmd "run" "Run hooks" (Run <$> Run.parser)

    serverCommand =
      mkCmd "server" "Run the web server" (Server <$> Server.parser)

-- | Main entry point.
main :: IO ()
main = do
  Options {..} <- execParser $ info (parser <**> helper) idm
  L.runLogger optionLogging $ do
    env <- Env.env optionConfigFile optionLogging
    usingReaderT env $
      case optionCommand of
        Create o -> Create.run o
        List o -> List.run o
        Run o -> Run.run o
        Server o -> Server.run o
