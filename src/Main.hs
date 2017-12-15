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
import Data.Default (def)
import Options.Applicative

--------------------------------------------------------------------------------
-- Local Imports:
import qualified UI.Create as Create
import qualified UI.List as List
import qualified Web.Hooks.Personal.Database as Database

--------------------------------------------------------------------------------
-- | Subcommand.
data Command = Create Create.Options -- ^ Create a new hook.
             | List List.Options     -- ^ List hooks.

--------------------------------------------------------------------------------
-- | Command line options
data Options = Options
  { optionCommand :: Command
    -- ^ Which subcommand to run.
  }

--------------------------------------------------------------------------------
-- | Command line option parser.
parser :: Parser Options
parser =
  Options
    <$> subparser (mconcat [ createCommand
                           , listCommand
                           ])

  where
    mkCmd :: String -> String -> Parser a -> Mod CommandFields a
    mkCmd name desc p =
      command name (info (p <**> helper) (progDesc desc))

    createCommand =
      mkCmd "create" "Create a new webhook" (Create <$> Create.parser)

    listCommand =
      mkCmd "list" "List hooks" (List <$> List.parser)

--------------------------------------------------------------------------------
-- | Main entry point.
main :: IO ()
main = do
  options <- execParser $ info (parser <**> helper) idm

  database <- Database.database def
  Database.migrate database False

  case optionCommand options of
    Create o -> Create.run o database
    List o   -> List.run o database
