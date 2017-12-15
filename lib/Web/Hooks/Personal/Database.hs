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
module Web.Hooks.Personal.Database
  ( Database.Database
  , Database.Config(..)
  , Database.database
  , Database.runQuery
  , Database.runInsert
  , Database.migrate
  ) where

--------------------------------------------------------------------------------
import qualified Web.Hooks.Personal.Database.Config as Database
import qualified Web.Hooks.Personal.Database.Internal as Database
