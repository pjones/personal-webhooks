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
module Web.Hooks.Personal.Internal.Database.Prim
  ( Database,
    Page (..),
    Rows (..),
    database,
    runQuery,
    runInsert,
    limit,
    migrate,
  )
where

import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PostgreSQL
import Database.PostgreSQL.Simple.Migration
  ( MigrationCommand (..),
    MigrationResult (..),
    runMigrations,
  )
import Database.PostgreSQL.Simple.Util (existsTable)
import qualified Opaleye
import Paths_personal_webhooks (getDataDir)
import System.FilePath ((</>))
import Web.Hooks.Personal.Internal.Database.Config
import Web.Hooks.Personal.Internal.Logging (MonadLog, logCriticalThenDie, logDebug)

-- | A database handle.
newtype Database = Database
  { dbPool :: Pool Connection
  }

newtype Rows
  = -- | Limit a query to n rows per page.
    Rows Int

newtype Page
  = -- | The page number to query.
    Page Int

-- | Given a configuration object, create a database handle.
database :: (MonadIO m) => Config -> m Database
database Config {..} = do
  pool <- liftIO (Pool.createPool connect close 1 timeout configPoolSize)
  pure (Database pool)
  where
    constr = encodeUtf8 configConnectionString
    connect = PostgreSQL.connectPostgreSQL constr
    close = PostgreSQL.close
    timeout = 30

-- | Internal function to get a connection out of the pool.
withConnection :: (MonadIO m) => Database -> (Connection -> IO a) -> m a
withConnection Database {..} = liftIO . Pool.withResource dbPool

-- | Run an Opaleye query (SELECT).
runQuery ::
  (MonadIO m, Default Opaleye.QueryRunner cs h) =>
  -- | A 'Database' object.
  Database ->
  -- | The query you want to execute.
  Opaleye.Query cs ->
  -- | Query results, converted into the correct type.
  m [h]
runQuery d q = withConnection d $ \c -> Opaleye.runQuery c q

-- | Run an Opaleye insert.
runInsert ::
  (MonadIO m, Default Opaleye.Constant h cs) =>
  -- | A 'Database' object.
  Database ->
  -- | The table to insert into.
  Opaleye.Table cs cs' ->
  -- | List of values to insert.  These will automatically be
  -- converted into the appropriate PostgreSQL columns types.
  [h] ->
  -- | Number of rows inserted.
  m Int64
runInsert d t hs =
  withConnection d $ \c ->
    Opaleye.runInsertMany c t (map Opaleye.constant hs)

-- | A wrapper around Opaleye's limit and offset features.
limit :: Page -> Rows -> Opaleye.Query a -> Opaleye.Query a
limit (Page x) (Rows y) = Opaleye.limit y . Opaleye.offset (x * y)

-- | Run the database migrations.  Exits the current process if there
-- is an error running the migrations.
migrate ::
  forall m.
  MonadIO m =>
  MonadLog m =>
  Database ->
  Bool ->
  m ()
migrate d verbose = do
  logDebug "checking database for possible schema upgrade"
  join (withConnection d go)
  where
    go :: Connection -> IO (m ())
    go c = do
      inited <- existsTable c "schema_migrations"
      datadir <- liftIO getDataDir

      let dir = datadir </> "data" </> "migrations"
          mi = [MigrationInitialization | not inited]
          ms = mi ++ [MigrationDirectory dir]

      result <-
        PostgreSQL.withTransaction c $
          runMigrations verbose c ms

      pure $ case result of
        MigrationSuccess ->
          logDebug "database schema updated successfully"
        MigrationError e ->
          logCriticalThenDie ("database schema update failed: " <> toText e)
