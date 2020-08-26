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
-- A reader environment to make imperative programming easier.
module Web.Hooks.Personal.Env
  ( Env (..),
    env,
  )
where

import Web.Hooks.Personal.Config (Config)
import qualified Web.Hooks.Personal.Config as Config
import Web.Hooks.Personal.Internal.Database.Prim (Database)
import qualified Web.Hooks.Personal.Internal.Database.Prim as Database

-- | Everything you need to use this library.
data Env = Env
  { -- | Master configuration.
    config :: !Config,
    -- | Verbose flag.
    verbose :: !Bool,
    -- | Database handle.
    database :: !Database
  }

-- | Create an environment.  The optional 'FilePath' is passed along
-- to the 'Config.load' function.
env :: (MonadIO m) => Maybe FilePath -> Bool -> m Env
env path vflag = do
  c <- Config.load path >>= either die pure
  d <- Database.database (Config.configDatabase c)

  Database.migrate d vflag

  pure
    Env
      { config = c,
        verbose = vflag,
        database = d
      }
