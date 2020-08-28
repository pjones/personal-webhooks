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
-- Actions that can be performed in response to an HTTP request.
module Web.Hooks.Personal.Internal.Action.Prim
  ( Action (..),
    normalize,
    run,
  )
where

import Control.Exception.Safe (MonadCatch, catch)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import System.Directory (doesFileExist, getFileSize, makeAbsolute)
import System.Timeout (timeout)
import Web.Hooks.Personal.Internal.Action.Config (Config)
import qualified Web.Hooks.Personal.Internal.Action.Config as Config
import Web.Hooks.Personal.Internal.Action.Status (Status (..), assert)
import Web.Hooks.Personal.Internal.JSON
import Web.Hooks.Personal.Internal.Logging (MonadLog, logDebug)
import Web.Hooks.Personal.Internal.Request.Prim

-- | The different kinds of actions.
data Action
  = -- | Append incoming data to an existing file.
    AppendFileAction !FilePath
  | -- | Dummy (or disabled) action.
    NoAction
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via GenericJSON Action

liftJSON ''Action

-- | Some actions need to be normalized because their fields were set
-- by a user whom shouldn't have to understand how to create a
-- well-formed 'Action'.  For example, file paths must be converted to
-- absolute paths.
normalize :: MonadIO m => Action -> m Action
normalize = \case
  AppendFileAction f ->
    AppendFileAction <$> liftIO (makeAbsolute f)
  NoAction ->
    pure NoAction

-- | Run an action.
run ::
  forall m.
  MonadIO m =>
  MonadCatch m =>
  MonadLog m =>
  -- | Action configuration.
  Config ->
  -- | Incoming data.
  Request ->
  -- | The action to run.
  Action ->
  -- | Result status.
  m Status
run Config.Config {..} r a =
  catch (action a) onError
  where
    onError :: SomeException -> m Status
    onError = show >>> Fail >>> pure

    action :: Action -> m Status
    action = \case
      AppendFileAction f -> do
        logDebug ("appending request data to " <> toText f)
        append f <&> either id id
      NoAction -> do
        logDebug "action is a no-op, skipping"
        pure Okay

    append :: FilePath -> m (Either Status Status)
    append file = runExceptT $ do
      -- Verify the file exists.
      exists <- liftIO (doesFileExist file)
      assert exists (Invalid $ "file doesn't exist: " ++ file)

      -- Ensure it won't grow bigger than allowed.
      let bs = Aeson.encode (Aeson.toJSON r) <> "\n"
          bsize = toInteger (LByteString.length bs)

      size <- liftIO (getFileSize file)
      assert
        (size + bsize <= configMaxFileSize)
        (Fail "file size exceeds configured maximum")

      -- Safe to append now.
      t <-
        liftIO $
          timeout (configWriteTimeout * 1000) $
            withFile file AppendMode (`LByteString.hPut` bs)

      -- Test to see if all data was written.
      case t of
        Just () -> pure Okay
        Nothing -> pure (Fail $ "timeout while writing to " ++ file)
