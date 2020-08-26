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
module Web.Hooks.Personal.Internal.Database.Config
  ( Config (..),
    defaultConfig,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))

data Config = Config
  { -- | libpq connection string.
    configConnectionString :: !Text,
    -- | Size of the database connection pool.
    configPoolSize :: !Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configConnectionString = "user=webhooks dbname=webhooks password=webhooks",
      configPoolSize = 5
    }

instance FromJSON Config where
  parseJSON = withObject "database config" $ \v ->
    Config
      <$> v .:? "connection" .!= configConnectionString
      <*> v .:? "poolSize" .!= configPoolSize
    where
      Config {..} = defaultConfig
