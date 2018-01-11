{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
module UI.Server
  ( Options
  , parser
  , run
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)
import Options.Applicative
import Snap.Core
import qualified Snap.Http.Server as HTTP

--------------------------------------------------------------------------------
-- Local Imports:
import qualified Web.Hooks.Personal.Action as Action
import qualified Web.Hooks.Personal.Config as Config
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import Web.Hooks.Personal.Hook (Hook)
import qualified Web.Hooks.Personal.Hook as Hook
import qualified Web.Hooks.Personal.Request as Request

--------------------------------------------------------------------------------
-- | Command line options.
data Options = Options
  { optionPort :: Int
  }

--------------------------------------------------------------------------------
-- | Command line parser.
parser :: Parser Options
parser =
  Options <$> option auto (mconcat [ long "port"
                                   , short 'p'
                                   , metavar "NUM"
                                   , help "The port number to bind to"
                                   ])

--------------------------------------------------------------------------------
-- | Fetch a hook from the database and run its action.
findAndRunHook :: (MonadIO m)
               => Env
               -> Text
               -> Request.Request
               -> MaybeT m Action.Status

findAndRunHook env code request = do
    a <- Hook.hookAction <$> findByCode
    Action.run actionConfig request a

  where
    findByCode :: (MonadIO m) => MaybeT m Hook
    findByCode = listToMaybeT =<<
                 Database.runQuery (Env.database env)
                   (Hook.findBy $ Hook.HookCode code)

    actionConfig :: Action.Config
    actionConfig = Config.configAction (Env.config env)

    listToMaybeT :: (Monad m) => [a] -> MaybeT m a
    listToMaybeT []    = mzero
    listToMaybeT (x:_) = return x

--------------------------------------------------------------------------------
-- | Routes.
site :: Env -> Snap ()
site env = route [ ("hooks/:hookcode", handler env) ]

--------------------------------------------------------------------------------
-- | Request handler.
handler :: Env -> Snap ()
handler env = do
    -- Review the request and try to run a hook.
    status <- runMaybeT $ do
      rq <- lift getRequest

      rdata <- -- Where the data comes from varies based on the request.
        case (rqMethod rq, getHeader "Content-Type" rq) of
          (GET,  _)                                        -> fromParams rq
          (POST, Just "application/x-www-form-urlencoded") -> fromParams rq
          (POST, Just "application/json")                  -> fromJSON
          (_, _) {- Anything else is a failure -}          -> mzero

      code <- MaybeT (getParam "hookcode")
      findAndRunHook env (Text.decodeUtf8 code) rdata

    -- Respond with the correct HTTP status code.
    modifyResponse . setResponseCode $
      case status of
        Nothing -> 404
        Just s  -> Action.statusToHTTP s

  where
    fromParams :: (Monad m) => Request -> MaybeT m Request.Request
    fromParams = MaybeT . return . Request.fromParams . rqParams

    fromJSON :: MaybeT Snap Request.Request
    fromJSON = MaybeT (Request.fromJSON <$> readRequestBody (maxBytes env))

    maxBytes :: Env -> Word64
    maxBytes = Request.configMaxBodySize . Config.configRequest . Env.config

--------------------------------------------------------------------------------
-- | Snap server configuration.
snapCfg :: MonadSnap m => Options -> HTTP.Config m a
snapCfg Options{..} =
    HTTP.setErrorLog  (HTTP.ConfigIoLog stdoutLog) $
    HTTP.setAccessLog (HTTP.ConfigIoLog stdoutLog) $
    HTTP.setPort      optionPort $
    HTTP.setBind      "127.0.0.1"
    HTTP.defaultConfig
  where
    stdoutLog :: ByteString -> IO ()
    stdoutLog = ByteString.putStr . (<> "\n")

--------------------------------------------------------------------------------
-- | Run the @server@ command to receive HTTP requests.
run :: (MonadIO m) => Options -> ReaderT Env m ()
run options = do
  env <- ask
  liftIO $ HTTP.httpServe (snapCfg options) (site env)
