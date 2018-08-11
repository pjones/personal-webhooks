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
module Web.Hooks.Personal.Internal.Util.Process
  ( die
  ) where

--------------------------------------------------------------------------------
import qualified System.Exit as Exit
import Control.Monad.IO.Class (MonadIO, liftIO)

--------------------------------------------------------------------------------
-- | Exit the current process with an error message.
die :: (MonadIO m) => String -> m a
die = liftIO . Exit.die . ("ERROR: " ++)
