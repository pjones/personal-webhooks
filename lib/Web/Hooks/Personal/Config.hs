{-# LANGUAGE OverloadedStrings #-}

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
-- | Overall configuration file type and parser.
module Web.Hooks.Personal.Config
  ( Config(..)
  , defaultPath
  , parseFile
  , load
  ) where

--------------------------------------------------------------------------------
-- Library imports:
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON(parseJSON), withObject, (.:?), (.!=))
import Data.Bifunctor (bimap)
import Data.Default (Default(def))
import qualified Data.Yaml as YAML
import System.Directory (XdgDirectory(..), getXdgDirectory, doesFileExist)

--------------------------------------------------------------------------------
-- Local imports:
import qualified Web.Hooks.Personal.Action.Config as Action
import qualified Web.Hooks.Personal.Database.Config as Database
import qualified Web.Hooks.Personal.Request.Config as Request

--------------------------------------------------------------------------------
-- | The master configuration structure containing all other
-- configuration types.
data Config = Config
  { configAction   :: Action.Config    -- ^ Action configuration.
  , configDatabase :: Database.Config  -- ^ Database configuration.
  , configRequest  :: Request.Config   -- ^ Request configuration.
  }

--------------------------------------------------------------------------------
instance Default Config where
  def = Config { configAction   = def
               , configDatabase = def
               , configRequest  = def
               }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = withObject "configuration" $ \v ->
    Config <$> v .:? "actions"  .!= def
           <*> v .:? "database" .!= def
           <*> v .:? "requests" .!= def

--------------------------------------------------------------------------------
-- | The path to the default configuration file.
defaultPath :: (MonadIO m) => m FilePath
defaultPath = liftIO (getXdgDirectory XdgConfig "webhooks")

--------------------------------------------------------------------------------
-- | Parse the given configuration file.
parseFile :: (MonadIO m) => FilePath -> m (Either String Config)
parseFile path = bimap show id <$> liftIO (YAML.decodeFileEither path)

--------------------------------------------------------------------------------
-- | Load the given file, or the default configuration file.
load :: (MonadIO m) => Maybe FilePath -> m (Either String Config)
load (Just path) = parseFile path
load Nothing     = do
  path <- defaultPath
  exists <- liftIO (doesFileExist path)

  if exists
    then parseFile path
    else return (Right def)
