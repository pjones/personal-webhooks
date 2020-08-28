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
module UI.List
  ( Options,
    parser,
    run,
  )
where

import qualified Data.Aeson as Aeson
import qualified Opaleye
import Options.Applicative
import qualified Text.Layout.Table as Table
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import qualified Web.Hooks.Personal.Hook as Hook

data Format = Table | JSON | Plain

-- | Options needed to list hooks
newtype Options = Options
  { optionFormat :: Format
  }

-- | Parse command line options for creating a new hook.
parser :: Parser Options
parser = Options <$> format
  where
    format = table <|> json <|> pure Plain

    table =
      flag'
        Table
        ( mconcat
            [ long "table",
              short 't',
              help "Format output as a table"
            ]
        )

    json =
      flag'
        JSON
        ( mconcat
            [ long "json",
              short 'j',
              help "Format output as JSON"
            ]
        )

formatted :: [Hook.Hook] -> IO ()
formatted hs =
  putStrLn $ Table.tableString specs Table.asciiS headers (map row hs)
  where
    specs = [Table.numCol, defSpec, defSpec, defSpec]
    defSpec = Table.column Table.expand Table.left Table.noAlign (Table.singleCutMark "…")
    headers = Table.titlesH ["ID", "Code", "Action", "Expires"]
    row Hook.Hook {..} =
      Table.rowG
        [ show hookID,
          toString hookCode,
          decodeUtf8 (Aeson.encode hookAction),
          maybe "" show hookExpirationTime
        ]

-- | Run the @list@ command to list existing hooks.
run :: (MonadIO m) => Options -> ReaderT Env m ()
run Options {..} = do
  db <- asks Env.database
  hooks <- Database.runQuery db query

  liftIO $ case optionFormat of
    Table -> formatted hooks
    JSON -> putLBSLn (Aeson.encode hooks)
    Plain -> forM_ hooks $ \Hook.Hook {..} ->
      putTextLn $
        mconcat $
          intersperse
            " "
            [ show hookID,
              hookCode,
              decodeUtf8 (Aeson.encode hookAction),
              maybe "Never Expires" show hookExpirationTime
            ]
  where
    query =
      Database.limit (Database.Page 0) (Database.Rows 100) $
        Opaleye.orderBy (Opaleye.asc Hook.hookID) $
          Hook.findWithExpired Hook.AllHooks
