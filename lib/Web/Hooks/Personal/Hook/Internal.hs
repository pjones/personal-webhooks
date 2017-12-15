{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

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
-- | Information about how to handle an HTTP request.
module Web.Hooks.Personal.Hook.Internal
  ( Hook
  , HookNotSaved
  , Hook'(..)
  , HookR
  , HookW
  , pHook
  , hook
  ) where

--------------------------------------------------------------------------------
-- Library Imports.
import qualified Codec.Binary.Base64Url as Base64
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random (randomBytesGenerate, drgNewSeed, seedNew)
import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int64)
import Data.Profunctor.Product.Default (Default(def))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Opaleye (Column, Nullable, Constant(..), constant)
import Opaleye.PGTypes

--------------------------------------------------------------------------------
-- Local Imports.
import Web.Hooks.Personal.Action.Internal (Action)

--------------------------------------------------------------------------------
-- | Type alias to make the @Hook'@ type concrete.
type HookNotSaved = Hook' (Maybe Int64) Text (Maybe UTCTime) Action
type Hook         = Hook' Int64 Text (Maybe UTCTime) Action

--------------------------------------------------------------------------------
-- | Polymorphic hook suitable for storing in a database.
data Hook' key text time action = Hook
  { hookID :: key
    -- ^ Database ID.

  , hookCode :: text
    -- ^ Secret code.

  , hookExpirationTime :: time
    -- ^ Optional expiration time for the hook.

  , hookAction :: action
    -- ^ The action to run for this hook.
  } deriving (Generic, Show)

--------------------------------------------------------------------------------
instance ToJSON Hook
instance FromJSON Hook

--------------------------------------------------------------------------------
-- | Create profunctor-product instances for Opaleye.
$(makeAdaptorAndInstance "pHook" ''Hook')

--------------------------------------------------------------------------------
-- | Concrete hook type for reading columns from the database.
type HookR = Hook' (Column PGInt8)                   -- ID
                   (Column PGText)                   -- Code
                   (Column (Nullable PGTimestamptz)) -- Expiration date.
                   (Column PGJson)                   -- Action (encoded as JSON)

--------------------------------------------------------------------------------
-- | Concrete hook type for writing columns to the database.
type HookW = Hook' (Maybe (Column PGInt8))           -- Auto increment ID.
                   (Column PGText)                   -- Code
                   (Column (Nullable PGTimestamptz)) -- Expiration date.
                   (Column PGJson)                   -- Action (encoded as JSON)

--------------------------------------------------------------------------------
instance Default Constant Hook ( Maybe (Column PGInt8)
                               , Column PGText
                               , Column (Nullable PGTimestamptz)
                               , Column PGJson
                               ) where
  def = Constant $ \Hook{..} -> ( Nothing
                                , constant hookCode
                                , constant hookExpirationTime
                                , constant hookAction
                                )

--------------------------------------------------------------------------------
-- | Helper function to construct a hook with a unique @hookCode@.
hook :: (MonadIO m) => Maybe UTCTime -> Action -> m HookNotSaved
hook time action = do
    bytes <- liftIO (fst . randomBytesGenerate 48 . drgNewSeed <$> seedNew)
    let code = Text.decodeUtf8 (Base64.encode bytes)
    return (Hook Nothing code time action)
