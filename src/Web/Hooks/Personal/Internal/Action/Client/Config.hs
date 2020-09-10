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
module Web.Hooks.Personal.Internal.Action.Client.Config
  ( Config (..),
    configParser,
  )
where

import qualified Options.Applicative as OA

-- | Client configuration.
data Config = Config
  { -- | Whether or not to allow unsafe operations.
    configForce :: !Bool,
    -- | When creating files, optionally set their group.
    configGroupName :: !(Maybe String)
  }

-- | Command line option parser for client configuration.
configParser :: OA.Parser Config
configParser =
  Config
    <$> OA.switch
      ( mconcat
          [ OA.long "force",
            OA.help "Allow unsafe actions such as deleting files"
          ]
      )
    <*> optional
      ( OA.strOption $
          mconcat
            [ OA.long "group",
              OA.metavar "NAME",
              OA.help "Set the group name for created files to NAME"
            ]
      )
