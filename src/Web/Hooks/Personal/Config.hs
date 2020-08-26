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
-- | Overall configuration file type and parser.
module Web.Hooks.Personal.Config
  ( Config (..),
    defaultConfig,
    defaultPath,
    parseFile,
    load,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:?))
import qualified Data.Yaml as YAML
import System.Directory (XdgDirectory (..), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import qualified Web.Hooks.Personal.Internal.Action.Config as Action
import qualified Web.Hooks.Personal.Internal.Database.Config as Database
import qualified Web.Hooks.Personal.Internal.Request.Config as Request

-- | The master configuration structure containing all other
-- configuration types.
data Config = Config
  { -- | Action configuration.
    configAction :: !Action.Config,
    -- | Database configuration.
    configDatabase :: !Database.Config,
    -- | Request configuration.
    configRequest :: !Request.Config
  }

defaultConfig :: Config
defaultConfig =
  Config
    { configAction = Action.defaultConfig,
      configDatabase = Database.defaultConfig,
      configRequest = Request.defaultConfig
    }

instance FromJSON Config where
  parseJSON = withObject "configuration" $ \v ->
    Config
      <$> v .:? "actions" .!= configAction
      <*> v .:? "database" .!= configDatabase
      <*> v .:? "requests" .!= configRequest
    where
      Config {..} = defaultConfig

-- | The path to the default configuration file.
defaultPath :: MonadIO m => m FilePath
defaultPath = do
  dir <- liftIO (getXdgDirectory XdgConfig "webhooks")
  pure (dir </> "config.yml")

-- | Parse the given configuration file.
parseFile :: MonadIO m => FilePath -> m (Either String Config)
parseFile =
  YAML.decodeFileEither
    >>> liftIO
    >>> (<&> first show)

-- | Load the given file, or the default configuration file.
load :: MonadIO m => Maybe FilePath -> m (Either String Config)
load (Just path) = parseFile path
load Nothing = do
  path <- defaultPath
  exists <- liftIO (doesFileExist path)
  if exists
    then parseFile path
    else pure (Right defaultConfig)
