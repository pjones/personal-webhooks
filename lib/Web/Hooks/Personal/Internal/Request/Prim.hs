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
module Web.Hooks.Personal.Internal.Request.Prim
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
-- | A type to represent the data sent with an HTTP request.
data Request = RequestParams (Map Text Value)
             | RequestJSON Aeson.Value

--------------------------------------------------------------------------------
-- | Helper type to collapse query parameters that only have one value.
data Value = Single Text | Multiple [Text]

--------------------------------------------------------------------------------
instance Aeson.ToJSON Request where
  toJSON (RequestParams p) = Aeson.toJSON p
  toJSON (RequestJSON v)   = v

--------------------------------------------------------------------------------
instance Aeson.ToJSON Value where
  toJSON (Single t)    = Aeson.String t
  toJSON (Multiple ts) = Aeson.toJSON ts

--------------------------------------------------------------------------------
-- | Create a 'Request' from a string containing JSON data.
fromJSON :: L.ByteString -> Maybe Request
fromJSON = fmap RequestJSON . Aeson.decode

--------------------------------------------------------------------------------
-- | Create a 'Request' from decoded query parameters.
fromParams :: Map B.ByteString [B.ByteString] -> Maybe Request
fromParams = Just . RequestParams . Map.fromList . map convert . Map.toList
  where
    convert :: (B.ByteString, [B.ByteString]) -> (Text, Value)
    convert (k, vs) = ( Text.decodeUtf8 k
                      , mkValue (map Text.decodeUtf8 vs)
                      )

    mkValue :: [Text] -> Value
    mkValue [x] = Single x
    mkValue xs  = Multiple xs
