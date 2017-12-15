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
-- | Command line option parser for 'Action'.
module Web.Hooks.Personal.Action.Options
  ( optionParser
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Options.Applicative

--------------------------------------------------------------------------------
-- Local Imports:
import Web.Hooks.Personal.Action.Internal (Action(..))

--------------------------------------------------------------------------------
-- | Parse an 'Action' from the command line.
optionParser :: Parser Action
optionParser = appendFileAction

--------------------------------------------------------------------------------
-- | Parse an 'AppendFileAction' from the command line.
appendFileAction :: Parser Action
appendFileAction =
  AppendFileAction
    <$> option str (mconcat [ long "append"
                            , metavar "FILE"
                            , help "Append the HTTP request to FILE"
                            ])
