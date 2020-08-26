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
-- Request data to pass along to a hook action.
module Web.Hooks.Personal.Request
  ( Request.Request,
    Request.fromJSON,
    Request.fromParams,
    Request.Config (..),
  )
where

import qualified Web.Hooks.Personal.Internal.Request.Config as Request
import qualified Web.Hooks.Personal.Internal.Request.Prim as Request
