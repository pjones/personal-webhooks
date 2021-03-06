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
module Web.Hooks.Personal.Database
  ( Database.Database,
    Database.Config (..),
    Database.Page (..),
    Database.Rows (..),
    Database.database,
    Database.runQuery,
    Database.runInsert,
    Database.limit,
    Database.migrate,
  )
where

import qualified Web.Hooks.Personal.Internal.Database.Config as Database
import qualified Web.Hooks.Personal.Internal.Database.Prim as Database
