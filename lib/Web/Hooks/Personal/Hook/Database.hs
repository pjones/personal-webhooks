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
module Web.Hooks.Personal.Hook.Database
  ( table
  , all
  , find
  ) where

--------------------------------------------------------------------------------
-- Library Imports:
import Control.Arrow (returnA)
import Data.Text (Text)
import Opaleye
import Prelude hiding (all)

--------------------------------------------------------------------------------
-- Local Imports:
import Web.Hooks.Personal.Database.Functions (now)
import Web.Hooks.Personal.Hook.Internal

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
-- | Fetch all hooks.
all :: Query HookR
all = queryTable table

--------------------------------------------------------------------------------
-- | Find a hook based on its secret code.
find :: Text -> Query HookR
find code = proc () -> do
    row <- queryTable table -< ()
    restrict -< hookCode row .== pgStrictText code
    restrict -< isNull (hookExpirationTime row) .|| notExpired row
    returnA  -< row

  where
    notExpired :: HookR -> Column PGBool
    notExpired row = fromNullable now (hookExpirationTime row) .< now
