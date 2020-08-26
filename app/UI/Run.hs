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
module UI.Run
  ( Options,
    parser,
    run,
  )
where

import qualified Data.ByteString.Lazy as LByteString
import Options.Applicative
import qualified Web.Hooks.Personal.Action as Action
import qualified Web.Hooks.Personal.Config as Config
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import Web.Hooks.Personal.Hook (Hook)
import qualified Web.Hooks.Personal.Hook as Hook
import Web.Hooks.Personal.Request (Request)
import qualified Web.Hooks.Personal.Request as Request

-- | How to read the request data.
data ReadFrom
  = ReadFromFile !FilePath
  | ReadFromStdin

data Options = Options
  { optionFind :: !Hook.FindBy,
    optionRead :: !ReadFrom
  }

parser :: Parser Options
parser =
  Options
    <$> Hook.parseFindBy
    <*> (readFromFile <|> readFromStdin)
  where
    readFromFile :: Parser ReadFrom
    readFromFile =
      ReadFromFile
        <$> strOption
          ( mconcat
              [ long "file",
                short 'f',
                metavar "FILE",
                help "Read request JSON from FILE"
              ]
          )

    readFromStdin :: Parser ReadFrom
    readFromStdin =
      flag'
        ReadFromStdin
        ( mconcat
            [ long "stdin",
              help "Read request JSON from STDIN"
            ]
        )

mkRequest :: ReadFrom -> IO Request
mkRequest readFrom = do
  raw <- case readFrom of
    ReadFromFile "-" -> LByteString.getContents
    ReadFromFile f -> readFileLBS f
    ReadFromStdin -> LByteString.getContents

  case Request.fromJSON raw of
    Nothing -> die "failed to parse JSON request data"
    Just r -> pure r

-- | Run the @run@ command to execute hooks.
run :: (MonadIO m) => Options -> ReaderT Env m ()
run Options {..} = do
  db <- asks Env.database
  request <- liftIO (mkRequest optionRead)
  hooks <- Database.runQuery db query
  status <- runHooks request hooks

  when
    (any (/= Action.Okay) status)
    (die "at least one hook action failed")
  where
    query = Hook.findBy optionFind

    runHooks ::
      MonadIO m =>
      Request ->
      [Hook] ->
      ReaderT Env m [Action.Status]
    runHooks request hooks = do
      config <- asks (Config.configAction . Env.config)
      forM hooks $ \hook -> do
        s <- Action.run config request (Hook.hookAction hook)
        putTextLn (Hook.hookCode hook <> " " <> show s)
        pure s
