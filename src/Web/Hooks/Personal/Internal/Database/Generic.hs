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
-- Use generic PostgreSQL columns to hold Haskell types.
module Web.Hooks.Personal.Internal.Database.Generic
  ( LiftJSON (..),
    liftJSON,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Profunctor.Product.Default (Default (def))
import Database.PostgreSQL.Simple.FromField
  ( Conversion,
    FromField (..),
    ResultError (..),
    returnError,
  )
import qualified Language.Haskell.TH as TH
import Opaleye hiding (FromField)

-- | A type wrapper to lift another type into PostgreSQL via @PGJson@.
newtype LiftJSON a = LiftJSON {unliftJSON :: a}

instance (FromJSON a, Typeable a) => FromField (LiftJSON a) where
  fromField f b = go =<< fromField f b
    where
      go :: Aeson.Value -> Conversion (LiftJSON a)
      go v = case Aeson.fromJSON v of
        Aeson.Success x -> pure (LiftJSON x)
        Aeson.Error e -> returnError ConversionFailed f e

instance (FromJSON a, Typeable a) => QueryRunnerColumnDefault PGJson (LiftJSON a) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance (ToJSON a) => Default Constant (LiftJSON a) (Column PGJson) where
  def = Constant (pgValueJSON . Aeson.toJSON . unliftJSON)

-- | Use Template Haskell to generate database instances for @PGJson@.
liftJSON :: TH.Name -> TH.Q [TH.Dec]
liftJSON name =
  [d|
    instance FromField $(TH.conT name) where
      fromField f b = unliftJSON <$> fromField f b

    instance QueryRunnerColumnDefault PGJson $(TH.conT name) where
      queryRunnerColumnDefault = unliftJSON <$> queryRunnerColumnDefault

    instance Default Constant $(TH.conT name) (Column PGJson) where
      def = Constant (pgValueJSON . Aeson.toJSON)
    |]
