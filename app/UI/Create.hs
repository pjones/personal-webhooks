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
module UI.Create
  ( Options,
    parser,
    run,
  )
where

import Options.Applicative
import qualified Web.Hooks.Personal.Action as Action
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import qualified Web.Hooks.Personal.Hook as Hook

-- | Options needed to create a new hook.
newtype Options = Options
  { optionAction :: Action.Action
  }

-- | Parse command line options for creating a new hook.
parser :: Parser Options
parser = Options <$> Action.optionParser

-- | Run the @create@ command to create a new webhook.
run :: (MonadIO m) => Options -> ReaderT Env m ()
run Options {..} = do
  d <- asks Env.database
  a <- Action.normalize optionAction
  h <- Hook.hook Nothing a
  _ <- Database.runInsert d Hook.table [h]
  putTextLn ("New hook code: " <> Hook.hookCode h)
