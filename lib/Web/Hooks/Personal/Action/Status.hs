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
-- | Status report after running an action.
module Web.Hooks.Personal.Action.Status
  ( Status(..)
  , assert
  , statusFromEither
  , statusToHTTP
  ) where

--------------------------------------------------------------------------------
import Control.Monad.Trans.Except (ExceptT(..), throwE)

--------------------------------------------------------------------------------
-- | Possible responses from running an action.
data Status = Okay           -- ^ Action ran successfully
            | Invalid String -- ^ Action can't run due to missing file/info.
            | Fail    String -- ^ Action failed.
            deriving (Eq, Show)

--------------------------------------------------------------------------------
assert :: (Monad m) => Bool -> Status -> ExceptT Status m ()
assert True  _ = return ()
assert False e = throwE e

--------------------------------------------------------------------------------
statusFromEither :: Either Status a -> Status
statusFromEither (Left s)  = s
statusFromEither (Right _) = Okay

--------------------------------------------------------------------------------
-- | Translate a 'Status' into an HTTP status code.
statusToHTTP :: Status -> Int
statusToHTTP Okay        = 204
statusToHTTP (Invalid _) = 501
statusToHTTP (Fail    _) = 500
