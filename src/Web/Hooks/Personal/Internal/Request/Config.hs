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
module Web.Hooks.Personal.Internal.Request.Config
  ( Config (..),
    defaultConfig,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))

data Config = Config
  { -- | Maximum number of bytes to read from an HTTP body.
    configMaxBodySize :: !Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configMaxBodySize = 2048 -- 2k
    }

instance FromJSON Config where
  parseJSON = withObject "request config" $ \v ->
    Config <$> v .:? "maxBodySize" .!= configMaxBodySize
    where
      Config {..} = defaultConfig
