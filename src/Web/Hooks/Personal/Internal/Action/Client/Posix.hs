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
module Web.Hooks.Personal.Internal.Action.Client.Posix
  ( manageNamedPipe,
  )
where

import Control.Exception (AllocationLimitExceeded (AllocationLimitExceeded))
import Control.Exception.Safe (bracket, catch, throwIO)
import qualified Data.ByteString.Char8 as CByteString
import qualified Data.ByteString.Unsafe as UByteString
import qualified Foreign as F
import GHC.Conc (closeFdWith, threadWaitRead)
import System.Directory (doesFileExist, removeFile)
import System.IO (openFile)
import qualified System.Posix as Posix
import Web.Hooks.Personal.Env (Env (..))
import Web.Hooks.Personal.Internal.Action.Client.Config
import Web.Hooks.Personal.Internal.Action.Status
import Web.Hooks.Personal.Internal.Logging (logDebug, logError, logInfo, runLoggerIO)

-- | Dispatch actions that use named pipes.
manageNamedPipe ::
  -- | Environment.
  Env ->
  -- | A function to send incoming bytes to.
  (ByteString -> IO ()) ->
  -- | Configuration.
  Config ->
  -- | The file to use as the named pipe.
  FilePath ->
  -- | Status when an error occurs.  Otherwise this function never
  -- returns.
  IO Status
manageNamedPipe env receiver Config {..} file =
  (`catch` onError) $ do
    exists <- doesFileExist file
    if exists && not configForce
      then pure (Fail $ "file already exists: " <> file)
      else go exists >> pure Okay
  where
    go :: Bool -> IO ()
    go exists = runLoggerIO (loggingH env) $ do
      when (exists && configForce) $ do
        logInfo ("removing file due to --force flag: " <> toText file)
        liftIO (removeFile file)
      logDebug ("creating named pipe: " <> toText file)
      liftIO $ bracket create (const remove) (const process)

    getOwnerAndGroup :: String -> IO (Posix.UserID, Posix.GroupID)
    getOwnerAndGroup groupName = do
      uid <- Posix.getEffectiveUserID
      gid <- Posix.getGroupEntryForName groupName <&> Posix.groupID
      pure (uid, gid)

    -- Create the named pipe with the correct permissions.
    create :: IO ()
    create = do
      ids <- traverse getOwnerAndGroup configGroupName
      Posix.createNamedPipe file 0O620
      case ids of
        Nothing -> pass
        Just (uid, gid) -> Posix.setOwnerAndGroup file uid gid

    -- Remove the named pipe from the file system.
    remove :: IO ()
    remove = runLoggerIO (loggingH env) $ do
      logDebug ("removing named pipe: " <> toText file)
      liftIO (removeFile file)

    -- Open a file, process it, and then close it; forever.
    process :: IO ()
    process = forever $ do
      (`catch` (onError >=> runLoggerIO (loggingH env) . logError . show)) $ do
        bracket open close (snd >>> read)
      where
        open = do
          h <- openFile file ReadMode
          fd <- Posix.handleToFd h
          pure (h, fd)
        close (_, fd) =
          closeFdWith Posix.closeFd fd

    -- Continue reading from a file as long as possible.
    read :: Posix.Fd -> IO ()
    read fd = do
      let bufferSize = 4096
      F.allocaBytes bufferSize $ \ptr ->
        let reader leftover = do
              let space = bufferSize - CByteString.length leftover
              when (space <= 0) (throwIO AllocationLimitExceeded)
              threadWaitRead fd
              count <- Posix.fdReadBuf fd ptr (fromIntegral space) <&> fromIntegral
              if count == 0
                then unless (CByteString.null leftover) (receiver leftover)
                else send count leftover
            send count prev = do
              bs <- UByteString.unsafePackCStringFinalizer ptr count pass
              let (lines, leftover) = wholeLines (prev <> bs)
              mapM_ receiver lines
              reader leftover
         in reader mempty

    -- Split on line boundaries and return lines along with any
    -- leftover bytes that did not end on a new line.
    wholeLines :: ByteString -> ([ByteString], ByteString)
    wholeLines = maybeSplit [] >>> first reverse
      where
        maybeSplit :: [ByteString] -> ByteString -> ([ByteString], ByteString)
        maybeSplit bs b
          | CByteString.null b = (bs, b)
          | otherwise =
            let (b0, b1) = CByteString.break (== '\n') b
             in if CByteString.take 1 b1 == "\n"
                  then maybeSplit (b0 <> "\n" : bs) (CByteString.drop 1 b1)
                  else (bs, b)

    onError :: SomeException -> IO Status
    onError = show >>> Fail >>> pure
