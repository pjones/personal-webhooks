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
-- | Access to PostgreSQL functions.
module Web.Hooks.Personal.Database.Functions
  ( now
  ) where

--------------------------------------------------------------------------------
import Opaleye.Internal.Column (Column(..))
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.PGTypes

--------------------------------------------------------------------------------
-- | Current transaction time.
now :: Column PGTimestamptz
now = Column (HPQ.FunExpr "now" [])
