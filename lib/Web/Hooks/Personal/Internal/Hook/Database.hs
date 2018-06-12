{-# LANGUAGE Arrows #-}

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
module Web.Hooks.Personal.Internal.Hook.Database
  ( table
  , findBy
  , findWithExpired
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Arrow (returnA)
import Opaleye hiding (table)

--------------------------------------------------------------------------------
-- Local Imports:
import Web.Hooks.Personal.Internal.Database.Functions (now)
import Web.Hooks.Personal.Internal.Hook.FindBy
import Web.Hooks.Personal.Internal.Hook.Prim

--------------------------------------------------------------------------------
-- | Opaleye defintion for the hooks table.
table :: Table HookW HookR
table = Table "hooks"
  (pHook Hook { hookID             = optional "id"
              , hookCode           = required "code"
              , hookExpirationTime = required "expires_at"
              , hookAction         = required "action"
              })

--------------------------------------------------------------------------------
-- | Restrict a query to only those rows identified by 'FindBy'.
findRestriction :: FindBy -> QueryArr HookR ()
findRestriction by = case by of
  HookID hid -> proc t -> restrict -< hookID t   .== pgInt8 hid
  HookCode c -> proc t -> restrict -< hookCode t .== pgStrictText c
  AllHooks   -> proc _ -> returnA  -< ()

--------------------------------------------------------------------------------
-- | Restrict a query so expired records are not returned.
notExpired :: QueryArr HookR ()
notExpired = proc t ->
    restrict -< isNull (hookExpirationTime t) .|| checkExpired t

  where
    checkExpired :: HookR -> Column PGBool
    checkExpired t = fromNullable now (hookExpirationTime t) .< now

--------------------------------------------------------------------------------
-- | Find a hook based on the given finder.
findWithExpired :: FindBy -> Query HookR
findWithExpired by = proc () -> do
  t <- queryTable table -< ()
  findRestriction by -< t
  returnA  -< t

--------------------------------------------------------------------------------
-- | Find a hook based on the given finder.  Restricts the output to
-- hooks that have not yet expired.
findBy :: FindBy -> Query HookR
findBy by = proc () -> do
  t <- findWithExpired by -< ()
  notExpired -< t
  returnA -< t
