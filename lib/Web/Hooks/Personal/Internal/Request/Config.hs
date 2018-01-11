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
module Web.Hooks.Personal.Internal.Request.Config
  ( Config(..)
  ) where

--------------------------------------------------------------------------------
import Data.Aeson (FromJSON(parseJSON), withObject, (.:?), (.!=))
import Data.Default (Default(def))
import Data.Word (Word64)

--------------------------------------------------------------------------------
data Config = Config
  { configMaxBodySize :: Word64
    -- ^ Maximum number of bytes to read from an HTTP body.
  }

--------------------------------------------------------------------------------
instance Default Config where
  def = Config
        { configMaxBodySize = 2048 -- 2k
        }

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON = withObject "request config" $ \v ->
    Config <$> v .:? "maxBodySize" .!= configMaxBodySize def
