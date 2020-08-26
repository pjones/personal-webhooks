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
module Web.Hooks.Personal.Internal.Hook.FindBy
  ( FindBy (..),
    parseFindBy,
  )
where

import Options.Applicative

-- | A way to describe to the database helpers how you want to find a
-- Hook that exists in the database.
data FindBy
  = -- | Using a database ID.
    HookID !Int64
  | -- | Using a secret code.
    HookCode !Text
  | -- | Use all rows.
    AllHooks

parseFindBy :: Parser FindBy
parseFindBy = usingHookID <|> usingHookCode <|> allHooks
  where
    usingHookID :: Parser FindBy
    usingHookID =
      fmap HookID $
        option auto $
          mconcat
            [ long "id",
              short 'i',
              metavar "ID",
              help "Limit to hook with ID"
            ]

    usingHookCode :: Parser FindBy
    usingHookCode =
      HookCode
        <$> strOption
          ( mconcat
              [ long "code",
                short 'c',
                metavar "CODE",
                help "Limit to hook with CODE"
              ]
          )

    allHooks :: Parser FindBy
    allHooks =
      flag' AllHooks $
        mconcat
          [ long "all",
            help "Apply to all hooks"
          ]
