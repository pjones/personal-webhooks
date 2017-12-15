{-# LANGUAGE RecordWildCards #-}

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
module UI.List
  ( Options
  , parser
  , run
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad (forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (def)
import qualified Data.Text as Text
import Options.Applicative
import qualified Text.Layout.Table as Table

--------------------------------------------------------------------------------
-- Local Imports:
import Web.Hooks.Personal.Database (Database)
import qualified Web.Hooks.Personal.Database as Database
import qualified Web.Hooks.Personal.Hook as Hook

--------------------------------------------------------------------------------
data Format = Table | JSON | Plain

--------------------------------------------------------------------------------
-- | Options needed to list hooks
data Options = Options
  { optionFormat :: Format
  }

--------------------------------------------------------------------------------
-- | Parse command line options for creating a new hook.
parser :: Parser Options
parser = Options <$> format
  where
    format = table <|> json <|> pure Plain

    table = flag' Table (mconcat [ long "table"
                                 , short 't'
                                 , help "Format output as a table"
                                 ])

    json = flag' JSON (mconcat [ long "json"
                               , short 'j'
                               , help "Format output as JSON"
                               ])

--------------------------------------------------------------------------------
formatted :: [Hook.Hook] -> IO ()
formatted hs =
    putStrLn $ Table.tableString specs Table.asciiS header (map row hs)

  where
    specs = [Table.numCol, def, def, def]
    header = Table.titlesH ["ID", "Code", "Action", "Expires"]
    row Hook.Hook{..} = Table.rowG [ show hookID
                                   , Text.unpack hookCode
                                   , show hookAction
                                   , maybe "" show hookExpirationTime
                                   ]

--------------------------------------------------------------------------------
-- | Run the @create@ command to create a new web hook.
run :: Options -> Database -> IO ()
run Options{..} db = do
  hooks <- Database.runQuery db Hook.all :: IO [Hook.Hook]

  case optionFormat of
    Table -> formatted hooks
    JSON  -> LBS.putStrLn (Aeson.encode hooks)
    Plain -> forM_ hooks $ \Hook.Hook{..} ->
               print (hookID, hookCode, hookAction, hookExpirationTime)
