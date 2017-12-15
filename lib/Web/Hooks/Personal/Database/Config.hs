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
module Web.Hooks.Personal.Database.Config
  ( Config(..)
  ) where

--------------------------------------------------------------------------------
import Data.Aeson (FromJSON(parseJSON), withObject, (.:?), (.!=))
import Data.Default (Default(def))
import Data.Text (Text)

--------------------------------------------------------------------------------
data Config = Config
  { configConnectionString :: Text
    -- ^ libpq connection string.

  , configPoolSize :: Int
    -- ^ Size of the database connection pool.
  }

--------------------------------------------------------------------------------
instance Default Config where
  def = Config
        { configConnectionString = "user=webhooks dbname=webhooks password=webhooks"
        , configPoolSize = 5
        }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = withObject "database config" $ \v ->
    Config <$> v .:? "connection" .!= configConnectionString def
           <*> v .:? "poolSize"   .!= configPoolSize def
