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
-- Actions for responding to HTTP requests.
module Web.Hooks.Personal.Action
  ( Action.Action (..),
    Action.Status (..),
    Action.Config (..),
    Action.normalize,
    Action.statusToHTTP,
    Action.run,
    Action.optionParser,
  )
where

import qualified Web.Hooks.Personal.Internal.Action.Config as Action
import qualified Web.Hooks.Personal.Internal.Action.Options as Action
import qualified Web.Hooks.Personal.Internal.Action.Prim as Action
import qualified Web.Hooks.Personal.Internal.Action.Status as Action
