{-# LANGUAGE CPP #-}

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
module Web.Hooks.Personal.Internal.Action.Client
  ( Config (..),
    configParser,
    dispatch,
  )
where

import Web.Hooks.Personal.Env (Env)
import Web.Hooks.Personal.Internal.Action.Client.Config
import Web.Hooks.Personal.Internal.Action.Prim
import Web.Hooks.Personal.Internal.Action.Status

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import Web.Hooks.Personal.Internal.Action.Client.Windows
#else
import Web.Hooks.Personal.Internal.Action.Client.Posix
#endif

-- | Respond to hooks by reading the incoming payload and sending it
-- to the given function.
dispatch ::
  Env ->
  -- | A function that will receive the payload from an action.  This
  -- function will be called each time the given action is fired.
  (ByteString -> IO ()) ->
  -- | Configuration.
  Config ->
  -- | The action to read payload bytes from.
  Action ->
  -- | The final status.  This function will only return if there is
  -- an error.  Otherwise it sits in a loop and dispatches incoming data.
  IO Status
dispatch env f c = \case
  AppendFileAction file -> manageNamedPipe env f c file
  NoAction -> pure (Invalid "the selected action has no associated data")
