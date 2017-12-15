{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
-- | Actions that can be performed in response to an HTTP request.
module Web.Hooks.Personal.Action.Internal
  ( Action(..)
  , Status(..)
  , statusToHTTP
  , run
  ) where

--------------------------------------------------------------------------------
-- Library Imports.
import Control.Exception (SomeException, catch)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getFileSize)
import System.IO (IOMode(AppendMode), withFile)

--------------------------------------------------------------------------------
-- Local Imports.
import Web.Hooks.Personal.Action.Config (Config)
import qualified Web.Hooks.Personal.Action.Config as Config
import Web.Hooks.Personal.Database.Generic (liftJSON)
import Web.Hooks.Personal.Request.Internal

--------------------------------------------------------------------------------
-- | The different kinds of actions.
data Action = AppendFileAction FilePath
              -- ^ Append incoming data to an existing file.

            | NoAction
              -- ^ Dummy action.

              deriving (Show, Generic)

--------------------------------------------------------------------------------
instance ToJSON Action
instance FromJSON Action
liftJSON ''Action

--------------------------------------------------------------------------------
-- | Possible responses from running an action.
data Status = Okay    -- ^ Action ran successfully
            | Invalid -- ^ Action can't run due to missing file/info.
            | Fail    -- ^ Action failed.

--------------------------------------------------------------------------------
-- | Translate a 'Status' into an HTTP status code.
statusToHTTP :: Status -> Int
statusToHTTP Okay    = 204
statusToHTTP Invalid = 501
statusToHTTP Fail    = 500

--------------------------------------------------------------------------------
-- | Run an action.
run :: (MonadIO m)
    => Config
    -- ^ Action configuration.

    -> Request
    -- ^ Incoming data.

    -> Action
    -- ^ The action to run.

    -> m Status
    -- ^ Result status.

run Config.Config{..} (Request v) a = do
  result <- liftIO $ catch (runMaybeT action)
                           (\(_ :: SomeException) -> return $ Just Fail)

  case result of
    Nothing -> return Invalid
    Just r  -> return r

  where
    action :: MaybeT IO Status
    action =
      case a of
        AppendFileAction f -> append f
        NoAction           -> return Okay
        -- Room to grow...

    append :: FilePath -> MaybeT IO Status
    append file = do
      -- Verify the file exists.
      exists <- liftIO (doesFileExist file)
      guard exists

      -- Ensure it won't grow bigger than allowed.
      let bs = encode v
          bsize = toInteger (LBS.length bs)

      size <- liftIO (getFileSize file)
      guard (size + bsize <= configMaxFileSize)

      -- Safe to append now.
      remaining <- liftIO $ withFile file AppendMode (`LBS.hPutNonBlocking` bs)

      -- Test to see if all data was written.
      if LBS.null remaining
         then return Okay
         else return Fail
