{-# LANGUAGE Arrows #-}

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
module Web.Hooks.Personal.Internal.Hook.Database
  ( table,
    findBy,
    findWithExpired,
  )
where

import Control.Arrow (returnA)
import qualified Opaleye as O
import Web.Hooks.Personal.Internal.Database.Functions (now)
import Web.Hooks.Personal.Internal.Hook.FindBy
import Web.Hooks.Personal.Internal.Hook.Prim

-- | Opaleye defintion for the hooks table.
table :: O.Table HookW HookR
table =
  O.table
    "hooks"
    ( pHook
        Hook
          { hookID = O.optional "id",
            hookCode = O.required "code",
            hookExpirationTime = O.required "expires_at",
            hookAction = O.required "action"
          }
    )

-- | Restrict a query to only those rows identified by 'FindBy'.
findRestriction :: FindBy -> O.SelectArr HookR ()
findRestriction by = case by of
  HookID hid -> proc t -> O.restrict -< hookID t O..== O.toFields hid
  HookCode c -> proc t -> O.restrict -< hookCode t O..== O.toFields c
  AllHooks -> proc _ -> returnA -< ()

-- | Restrict a query so expired records are not returned.
notExpired :: O.SelectArr HookR ()
notExpired = proc t ->
  O.restrict -< O.isNull (hookExpirationTime t) O..|| checkExpired t
  where
    checkExpired :: HookR -> O.Field O.SqlBool
    checkExpired t = O.fromNullable now (hookExpirationTime t) O..< now

-- | Find a hook based on the given finder.
findWithExpired :: FindBy -> O.Select HookR
findWithExpired by = proc () -> do
  t <- O.selectTable table -< ()
  findRestriction by -< t
  returnA -< t

-- | Find a hook based on the given finder.  Restricts the output to
-- hooks that have not yet expired.
findBy :: FindBy -> O.Select HookR
findBy by = proc () -> do
  t <- findWithExpired by -< ()
  notExpired -< t
  returnA -< t
