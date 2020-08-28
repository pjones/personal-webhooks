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
-- Status report after running an action.
module Web.Hooks.Personal.Internal.Action.Status
  ( Status (..),
    assert,
    statusToHTTP,
  )
where

import Control.Monad.Except (MonadError, throwError)
import qualified Network.HTTP.Types.Status as HTTP

-- | Possible responses from running an action.
data Status
  = -- | Action ran successfully
    Okay
  | -- | Action can't run due to missing file/info.
    Invalid !String
  | -- | Action failed.
    Fail !String
  deriving (Eq, Show)

-- | Fail unless an assertion holds.
assert :: MonadError Status m => Bool -> Status -> m ()
assert True _ = pass
assert False e = throwError e

-- | Translate a 'Status' into an HTTP status code.
statusToHTTP :: Status -> HTTP.Status
statusToHTTP Okay = HTTP.status204
statusToHTTP (Invalid _) = HTTP.status501
statusToHTTP (Fail _) = HTTP.status500
