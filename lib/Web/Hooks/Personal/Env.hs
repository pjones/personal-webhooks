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
-- | A reader environment to make imperative programming easier.
module Web.Hooks.Personal.Env
  ( Env(..)
  , env
  , die
  ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.Exit as Exit

--------------------------------------------------------------------------------
-- Local imports:
import Web.Hooks.Personal.Config (Config)
import qualified Web.Hooks.Personal.Config as Config
import Web.Hooks.Personal.Internal.Database.Prim (Database)
import qualified Web.Hooks.Personal.Internal.Database.Prim as Database

--------------------------------------------------------------------------------
-- | Everything you need to use this library.
data Env = Env
  { config   :: Config    -- ^ Master configuration.
  , database :: Database  -- ^ Database handle.
  }

--------------------------------------------------------------------------------
-- | Create an environment.  The optional 'FilePath' is passed along
-- to the 'Config.load' function.
env :: (MonadIO m) => Maybe FilePath -> m Env
env path = do
    c <- loadCfg
    d <- Database.database (Config.configDatabase c)

    Database.migrate d False

    return Env { config   = c
               , database = d
               }
  where
    loadCfg = do
      c <- Config.load path

      case c of
        Left e   -> die e
        Right c' -> return c'

--------------------------------------------------------------------------------
-- | Exit the current process with an error message.
die :: (MonadIO m) => String -> m a
die = liftIO . Exit.die . ("ERROR: " ++)
