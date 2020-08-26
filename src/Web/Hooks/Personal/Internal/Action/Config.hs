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
module Web.Hooks.Personal.Internal.Action.Config
  ( Config (..),
    defaultConfig,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))

data Config = Config
  { -- | Maximum number of bytes to all a file to grow before aborting.
    configMaxFileSize :: !Integer,
    -- | Maximum number of milliseconds (1/10^3) to wait for file
    -- writes to complete.  Useful to prevent a hung web server
    -- waiting for a dead FIFO script.
    configWriteTimeout :: !Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configMaxFileSize = 1048576, -- 1MB
      configWriteTimeout = 1000 -- 1 second
    }

instance FromJSON Config where
  parseJSON = withObject "action config" $ \v ->
    Config
      <$> v .:? "maxFileSize" .!= configMaxFileSize
      <*> v .:? "writeTimeout" .!= configWriteTimeout
    where
      Config {..} = defaultConfig
