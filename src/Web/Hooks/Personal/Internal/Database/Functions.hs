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
-- Access to PostgreSQL functions.
module Web.Hooks.Personal.Internal.Database.Functions
  ( now,
  )
where

import Opaleye.Internal.Column (Column (..))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.SqlTypes

-- | Current transaction time.
now :: Column SqlTimestamptz
now = Column (HPQ.FunExpr "now" [])
