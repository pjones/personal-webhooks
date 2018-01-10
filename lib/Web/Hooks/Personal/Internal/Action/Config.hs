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
module Web.Hooks.Personal.Internal.Action.Config
  ( Config(..)
  ) where

--------------------------------------------------------------------------------
import Data.Aeson (FromJSON(parseJSON), withObject, (.:?), (.!=))
import Data.Default (Default(def))

--------------------------------------------------------------------------------
data Config = Config
  { configMaxFileSize :: Integer
    -- ^ Maximum number of bytes to all a file to grow before aborting.

  , configWriteTimeout :: Int
    -- ^ Maximum number of milliseconds (1/10^3) to wait for file
    -- writes to complete.  Useful to prevent a hung web server
    -- waiting for a dead FIFO script.
  }

--------------------------------------------------------------------------------
instance Default Config where
  def = Config
        { configMaxFileSize  = 1048576 -- 1MB
        , configWriteTimeout = 1000    -- 1 second
        }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = withObject "action config" $ \v ->
    Config <$> v .:? "maxFileSize"  .!= configMaxFileSize def
           <*> v .:? "writeTimeout" .!= configWriteTimeout def
