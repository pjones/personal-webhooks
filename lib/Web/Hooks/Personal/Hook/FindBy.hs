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
module Web.Hooks.Personal.Hook.FindBy
  ( FindBy(..)
  , parseFindBy
  ) where

--------------------------------------------------------------------------------
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Options.Applicative

--------------------------------------------------------------------------------
-- | A way to describe to the database helpers how you want to find a
-- Hook that exists in the database.
data FindBy = HookID Int64   -- ^ Using a database ID.
            | HookCode Text  -- ^ Using a secret code.
            | AllHooks       -- ^ Use all rows.

--------------------------------------------------------------------------------
parseFindBy :: Parser FindBy
parseFindBy = usingHookID <|> usingHookCode <|> allHooks
  where
    usingHookID :: Parser FindBy
    usingHookID = fmap HookID $
      option auto $ mconcat [ long "id"
                            , short 'i'
                            , metavar "ID"
                            , help "Limit to hook with ID"
                            ]

    usingHookCode :: Parser FindBy
    usingHookCode = fmap (HookCode . Text.pack) $
      option str $ mconcat [ long "code"
                           , short 'c'
                           , metavar "CODE"
                           , help "Limit to hook with CODE"
                           ]

    allHooks :: Parser FindBy
    allHooks =
      flag' AllHooks $ mconcat [ long "all"
                               , help "Apply to all hooks"
                               ]
