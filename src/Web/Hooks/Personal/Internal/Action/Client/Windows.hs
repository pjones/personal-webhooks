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
module Web.Hooks.Personal.Internal.Action.Client.Windows
  ( manageNamedPipe,
  )
where

import Web.Hooks.Personal.Env (Env)
import Web.Hooks.Personal.Internal.Action.Client.Config
import Web.Hooks.Personal.Internal.Action.Status

-- | Not implemented.
manageNamedPipe ::
  Env ->
  (ByteString -> IO ()) ->
  Config ->
  FilePath ->
  IO Status
manageNamedPipe _ _ _ _ =
  pure (Invalid "named pipes on Windows are not supported")
