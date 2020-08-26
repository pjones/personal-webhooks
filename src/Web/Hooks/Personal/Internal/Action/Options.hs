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
--
-- Command line option parser for 'Action'.
module Web.Hooks.Personal.Internal.Action.Options
  ( optionParser,
  )
where

import Options.Applicative
import Web.Hooks.Personal.Internal.Action.Prim (Action (..))

-- | Parse an 'Action' from the command line.
optionParser :: Parser Action
optionParser = appendFileAction

-- | Parse an 'AppendFileAction' from the command line.
appendFileAction :: Parser Action
appendFileAction =
  AppendFileAction
    <$> strOption
      ( mconcat
          [ long "append",
            metavar "FILE",
            help "Append the HTTP request to FILE"
          ]
      )
