{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
module Web.Hooks.Personal.Internal.Action.Prim
  ( Action(..)
  , normalize
  , run
  ) where

--------------------------------------------------------------------------------
-- Library Imports.
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import System.Directory (makeAbsolute, doesFileExist, getFileSize)
import System.IO (IOMode(AppendMode), withFile)
import System.Timeout (timeout)

--------------------------------------------------------------------------------
-- Local Imports.
import Web.Hooks.Personal.Internal.Action.Config (Config)
import qualified Web.Hooks.Personal.Internal.Action.Config as Config
import Web.Hooks.Personal.Internal.Action.Status (Status(..), assert, statusFromEither)
import Web.Hooks.Personal.Internal.Database.Generic (liftJSON)
import Web.Hooks.Personal.Internal.Request.Prim

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
-- | Some actions need to be normalized because their fields were set
-- by a user whom shouldn't have to understand how to create a
-- well-formed 'Action'.  For example, file paths must be converted to
-- absolute paths.
normalize :: (MonadIO m) => Action -> m Action
normalize (AppendFileAction f) = AppendFileAction <$> liftIO (makeAbsolute f)
normalize NoAction             = return NoAction

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

run Config.Config{..} (Request v) a =
  statusFromEither <$> liftIO (catch (runExceptT action) handleE)

  where
    handleE :: SomeException -> IO (Either Status a)
    handleE e = return . Left . Fail $ show e

    action :: ExceptT Status IO ()
    action =
      case a of
        AppendFileAction f -> append f
        NoAction           -> return ()
        -- Room to grow...

    append :: FilePath -> ExceptT Status IO ()
    append file = do
      -- Verify the file exists.
      exists <- liftIO (doesFileExist file)
      assert exists (Invalid $ "file doesn't exist: " ++ file)

      -- Ensure it won't grow bigger than allowed.
      let bs = encode v <> "\n"
          bsize = toInteger (LBS.length bs)

      size <- liftIO (getFileSize file)
      assert (size + bsize <= configMaxFileSize)
             (Fail "file size exceeds configured maximum")

      -- Safe to append now.
      t <- liftIO $ timeout (configWriteTimeout * 1000) $
        withFile file AppendMode (`LBS.hPut` bs)

      -- Test to see if all data was written.
      assert (isJust t) (Fail $ "timeout while writing to " ++ file)
