{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

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
module Web.Hooks.Personal.Database.Internal
  ( Database
  , Page(..)
  , Rows(..)
  , database
  , runQuery
  , runInsert
  , limit
  , migrate
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Profunctor.Product.Default (Default)
import qualified Data.Text.Encoding as Text
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Opaleye
import System.Exit (die)
import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- For database migrations:
import Database.PostgreSQL.Simple.Util (existsTable)
import Database.PostgreSQL.Simple.Migration ( MigrationCommand(..)
                                            , MigrationResult(..)
                                            , runMigrations
                                            )

--------------------------------------------------------------------------------
-- Local Imports:
import Paths_personal_webhooks (getDataDir)
import Web.Hooks.Personal.Database.Config

--------------------------------------------------------------------------------
-- | A database handle.
data Database = Database
  { dbPool :: Pool Connection
  }

--------------------------------------------------------------------------------
newtype Rows = Rows Int -- ^ Limit a query to n rows per page.
newtype Page = Page Int -- ^ The page number to query.

--------------------------------------------------------------------------------
-- | Given a configuration object, create a database handle.
database :: (MonadIO m) => Config -> m Database
database Config{..} = do
    pool <- liftIO (Pool.createPool connect close 1 timeout configPoolSize)
    return (Database pool)
  where
    constr  = Text.encodeUtf8 configConnectionString
    connect = PostgreSQL.connectPostgreSQL constr
    close   = PostgreSQL.close
    timeout = 30

--------------------------------------------------------------------------------
-- | Internal function to get a connection out of the pool.
withConnection :: (MonadIO m) => Database -> (Connection -> IO a) -> m a
withConnection Database{..} = liftIO . Pool.withResource dbPool

--------------------------------------------------------------------------------
-- | Run an Opaleye query (SELECT).
runQuery :: (MonadIO m, Default Opaleye.QueryRunner cs h)
         => Database
         -- ^ A 'Database' object.

         -> Opaleye.Query cs
         -- ^ The query you want to execute.

         -> m [h]
         -- ^ Query results, converted into the correct type.

runQuery d q = withConnection d $ \c -> Opaleye.runQuery c q

--------------------------------------------------------------------------------
-- | Run an Opaleye insert.
runInsert :: (MonadIO m, Default Opaleye.Constant h cs)
          => Database
          -- ^ A 'Database' object.

          -> Opaleye.Table cs cs'
          -- ^ The table to insert into.

          -> [h]
          -- ^ List of values to insert.  These will automatically be
          -- converted into the appropriate PostgreSQL columns types.

          -> m Int64
          -- ^ Number of rows inserted.

runInsert d t hs =
  withConnection d $ \c ->
    Opaleye.runInsertMany c t (map Opaleye.constant hs)

--------------------------------------------------------------------------------
-- | A wrapper around Opaleye's limit and offset features.
limit :: Page -> Rows -> Opaleye.Query a -> Opaleye.Query a
limit (Page x) (Rows y) = Opaleye.limit y . Opaleye.offset (x * y)

--------------------------------------------------------------------------------
-- | Run the database migrations.  Exits the current process if there
-- is an error running the migrations.
migrate :: (MonadIO m) => Database -> Bool -> m ()
migrate d verbose = withConnection d go
  where
    go :: Connection -> IO ()
    go c = do
      inited  <- existsTable c "schema_migrations"
      datadir <- liftIO getDataDir

      let dir = datadir </> "data" </> "migrations"
          mi  = if inited then [] else [MigrationInitialization]
          ms  = mi ++ [MigrationDirectory dir]

      result <- PostgreSQL.withTransaction c $
                  runMigrations verbose c ms

      case result of
        MigrationSuccess -> return ()
        MigrationError e -> die e
