{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the package personal-webhooks. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at:

  git://git.devalot.com/personal-webhooks.git

No part of this package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.Trans.Reader (runReaderT)
import Options.Applicative

--------------------------------------------------------------------------------
-- Local Imports:
import qualified UI.Create as Create
import qualified UI.List as List
import qualified UI.Run as Run
import qualified UI.Server as Server
import qualified Web.Hooks.Personal.Env as Env

--------------------------------------------------------------------------------
-- | Subcommand.
data Command = Create Create.Options -- ^ Create a new hook.
             | List List.Options     -- ^ List hooks.
             | Run Run.Options       -- ^ Run hooks.
             | Server Server.Options -- ^ Web server.

--------------------------------------------------------------------------------
-- | Command line options
data Options = Options
  { optionConfigFile :: Maybe FilePath
    -- ^ Alternate configuration file to load.

  , optionCommand :: Command
    -- ^ Which subcommand to run.
  }

--------------------------------------------------------------------------------
-- | Command line option parser.
parser :: Parser Options
parser =
  Options
    <$> optional
          (option str
            (mconcat [ long "config"
                     , short 'c'
                     , metavar "FILE"
                     , help "Load FILE as an alternate config file"
                     ]))

    <*> subparser (mconcat [ createCommand
                           , listCommand
                           , runCommand
                           , serverCommand
                           ])

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

--------------------------------------------------------------------------------
-- | Main entry point.
main :: IO ()
main = do
  options <- execParser $ info (parser <**> helper) idm
  env <- Env.env (optionConfigFile options)

  flip runReaderT env $
    case optionCommand options of
      Create o -> Create.run o
      List o   -> List.run o
      Run o    -> Run.run o
      Server o -> Server.run o
