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
-- | Internal implementation of the 'Request' type and functions.
module Web.Hooks.Personal.Request.Internal
  ( Request(..)
  , fromJSON
  , fromParams
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

--------------------------------------------------------------------------------
-- | A type to represent an HTTP request.
newtype Request = Request Aeson.Value

--------------------------------------------------------------------------------
-- | Create a 'Request' from a string containing JSON data.
fromJSON :: L.ByteString -> Maybe Request
fromJSON = fmap Request . Aeson.decode

--------------------------------------------------------------------------------
-- | Create a 'Request' from decoded query parameters.
fromParams :: Map B.ByteString [B.ByteString] -> Maybe Request
fromParams =
    Just . Request . Aeson.toJSON . Map.fromList . map convert . Map.toList
  where
    convert :: (B.ByteString, [B.ByteString]) -> (Text, [Text])
    convert (k, vs) = (Text.decodeUtf8 k, map Text.decodeUtf8 vs)
