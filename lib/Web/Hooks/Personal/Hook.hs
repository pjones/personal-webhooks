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
-- | Hook type and functions.
module Web.Hooks.Personal.Hook
  ( Hook.Hook
  , Hook.HookNotSaved
  , Hook.Hook'(..)
  , Hook.hook
  , Hook.table
  , Hook.all
  , Hook.find
  ) where

--------------------------------------------------------------------------------
import qualified Web.Hooks.Personal.Hook.Database as Hook
import qualified Web.Hooks.Personal.Hook.Internal as Hook
