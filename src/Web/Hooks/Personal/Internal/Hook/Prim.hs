{-# LANGUAGE TemplateHaskell #-}

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
-- Information about how to handle an HTTP request.
module Web.Hooks.Personal.Internal.Hook.Prim
  ( Hook,
    HookNotSaved,
    Hook' (..),
    HookR,
    HookW,
    pHook,
    hook,
  )
where

import Crypto.Random (drgNewSeed, randomBytesGenerate, seedNew)
import Data.ByteArray.Encoding as Bytes
import Data.Profunctor.Product.Default (Default (def))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time (UTCTime)
import Opaleye (Column, Constant (..), Nullable, constant)
import Opaleye.PGTypes
import Web.Hooks.Personal.Internal.Action.Prim (Action)
import Web.Hooks.Personal.Internal.JSON

-- | Type alias to make the @Hook'@ type concrete.
type HookNotSaved = Hook' (Maybe Int64) Text (Maybe UTCTime) Action

type Hook = Hook' Int64 Text (Maybe UTCTime) Action

-- | Polymorphic hook suitable for storing in a database.
data Hook' key text time action = Hook
  { -- | Database ID.
    hookID :: !key,
    -- | Secret code.
    hookCode :: !text,
    -- | Optional expiration time for the hook.
    hookExpirationTime :: !time,
    -- | The action to run for this hook.
    hookAction :: !action
  }
  deriving (Generic, Show)

deriving via (GenericJSON Hook) instance ToJSON Hook

deriving via (GenericJSON Hook) instance FromJSON Hook

-- | Create profunctor-product instances for Opaleye.
$(makeAdaptorAndInstance "pHook" ''Hook')

-- | Concrete hook type for reading columns from the database.
type HookR =
  Hook'
    (Column PGInt8) -- ID
    (Column PGText) -- Code
    (Column (Nullable PGTimestamptz)) -- Expiration date.
    (Column PGJson) -- Action (encoded as JSON)

-- | Concrete hook type for writing columns to the database.
type HookW =
  Hook'
    (Maybe (Column PGInt8)) -- Auto increment ID.
    (Column PGText) -- Code
    (Column (Nullable PGTimestamptz)) -- Expiration date.
    (Column PGJson) -- Action (encoded as JSON)

instance
  Default
    Constant
    Hook
    ( Maybe (Column PGInt8),
      Column PGText,
      Column (Nullable PGTimestamptz),
      Column PGJson
    )
  where
  def = Constant $ \Hook {..} ->
    ( Nothing,
      constant hookCode,
      constant hookExpirationTime,
      constant hookAction
    )

-- | Helper function to construct a hook with a unique @hookCode@.
hook :: (MonadIO m) => Maybe UTCTime -> Action -> m HookNotSaved
hook time action = do
  bytes <- liftIO (fst . randomBytesGenerate 48 . drgNewSeed <$> seedNew)
  let code = Bytes.convertToBase Bytes.Base64URLUnpadded (bytes :: ByteString)
  pure (Hook Nothing (decodeUtf8 (code :: ByteString)) time action)
