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
module UI.Server
  ( Options,
    parser,
    run,
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict as HashMap
import Data.List (lookup)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative as OA
import qualified Web.Hooks.Personal.Action as Action
import qualified Web.Hooks.Personal.Config as Config
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import Web.Hooks.Personal.Hook (Hook)
import qualified Web.Hooks.Personal.Hook as Hook
import qualified Web.Hooks.Personal.Request as Request

-- | Command line options.
newtype Options = Options
  { optionPort :: Int
  }

-- | Command line parser.
parser :: OA.Parser Options
parser =
  Options
    <$> OA.option
      OA.auto
      ( mconcat
          [ OA.long "port",
            OA.short 'p',
            OA.metavar "NUM",
            OA.help "The port number to bind to"
          ]
      )

-- | Fetch a hook from the database and run its action.
findAndRunHook ::
  MonadIO m =>
  Env ->
  Text ->
  Request.Request ->
  MaybeT m Action.Status
findAndRunHook env code request = do
  a <- findByCode <&> Hook.hookAction
  Action.run actionConfig request a
  where
    findByCode :: (MonadIO m) => MaybeT m Hook
    findByCode =
      Database.runQuery
        (Env.database env)
        (Hook.findBy $ Hook.HookCode code)
        >>= (listToMaybe >>> hoistMaybe)

    actionConfig :: Action.Config
    actionConfig = Config.configAction (Env.config env)

-- | Request handler.
app :: Env -> Wai.Application
app env request respond = do
  code <- go <&> maybe HTTP.badRequest400 Action.statusToHTTP
  respond (Wai.responseBuilder code mempty mempty)
  where
    go :: MonadIO m => m (Maybe Action.Status)
    go = runMaybeT $ do
      let method = Wai.requestMethod request
          headers = Wai.requestHeaders request
          contentType = lookup "Content-Type" headers
      rdata <-
        case (method, contentType) of
          ("GET", _) ->
            pure (fromParams $ Wai.queryString request)
          ("POST", Just "application/x-www-form-urlencoded") ->
            readBody
              >>= ( toStrict
                      >>> HTTP.parseQuery
                      >>> fromParams
                      >>> pure
                  )
          ("POST", Just "application/json") ->
            readBody
              >>= ( Request.fromJSON
                      >>> hoistMaybe
                  )
          _ -> empty
      code <- hoistMaybe hookCode
      findAndRunHook env code rdata

    -- Read the body of the request, honoring the @maxBodySize@
    -- configuration setting.
    readBody :: MonadIO m => MaybeT m LByteString
    readBody =
      let maxBytes =
            Request.configMaxBodySize
              (Config.configRequest $ Env.config env)
          go bs n = do
            chunk <- liftIO (Wai.getRequestBodyChunk request)
            let cn = ByteString.length chunk
            guard (n + cn <= maxBytes)
            if cn == 0
              then pure bs
              else go (bs <> toLazy chunk) (n + cn)
       in go mempty 0

    -- Turn HTTP query parameters into a 'Request.Request'.
    fromParams :: HTTP.Query -> Request.Request
    fromParams =
      foldr
        ( \(key, value) ->
            HashMap.insertWith (<>) key (maybe mempty one value)
        )
        mempty
        >>> Request.fromParams

    -- Extract the hook code from the URL.
    hookCode :: Maybe Text
    hookCode =
      case Wai.pathInfo request of
        "hooks" : code : _ -> Just code
        _anyOtherPath -> Nothing

-- | Run the @server@ command to receive HTTP requests.
run :: MonadIO m => Options -> ReaderT Env m ()
run Options {..} = do
  env <- ask
  let settings =
        Warp.defaultSettings
          & Warp.setHost "127.0.0.1"
          & Warp.setPort optionPort
  liftIO (Warp.runSettings settings (app env))
