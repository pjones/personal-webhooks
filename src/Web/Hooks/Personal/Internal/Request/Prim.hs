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
--
-- Internal implementation of the 'Request' type and functions.
module Web.Hooks.Personal.Internal.Request.Prim
  ( Request (..),
    fromJSON,
    fromParams,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

-- | A type to represent the data sent with an HTTP request.
data Request
  = RequestParams !(HashMap Text Value)
  | RequestJSON !Aeson.Value

-- | Helper type to collapse query parameters that only have one value.
data Value
  = Single !Text
  | Multiple ![Text]

instance Aeson.ToJSON Request where
  toJSON (RequestParams p) = Aeson.toJSON p
  toJSON (RequestJSON v) = v

instance Aeson.ToJSON Value where
  toJSON (Single t) = Aeson.String t
  toJSON (Multiple ts) = Aeson.toJSON ts

-- | Create a 'Request' from a string containing JSON data.
fromJSON :: LByteString -> Maybe Request
fromJSON = fmap RequestJSON . Aeson.decode

-- | Create a 'Request' from decoded query parameters.
fromParams :: HashMap ByteString [ByteString] -> Request
fromParams =
  HashMap.foldrWithKey (\k v -> uncurry HashMap.insert (convert k v)) mempty
    >>> RequestParams
  where
    convert :: ByteString -> [ByteString] -> (Text, Value)
    convert k vs =
      ( decodeUtf8 k,
        mkValue (map decodeUtf8 vs)
      )

    mkValue :: [Text] -> Value
    mkValue [x] = Single x
    mkValue xs = Multiple xs
