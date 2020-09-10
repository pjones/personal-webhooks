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
module UI.Client
  ( Options,
    parser,
    run,
  )
where

import Control.Concurrent.Async (async, mapConcurrently_)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (bracket, throwString)
import qualified Data.ByteString as ByteString
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))
import System.IO (hClose)
import qualified System.Process as Process
import qualified Web.Hooks.Personal.Action as Action
import qualified Web.Hooks.Personal.Database as Database
import Web.Hooks.Personal.Env (Env)
import qualified Web.Hooks.Personal.Env as Env
import qualified Web.Hooks.Personal.Hook as Hook
import qualified Web.Hooks.Personal.Internal.Action.Client as Client
import Web.Hooks.Personal.Internal.Logging
  ( MonadLog,
    logDebug,
    logError,
    logInfo,
    runLoggerIO,
  )

-- | Command line options:
data Options = Options
  { -- | How to find the hooks we are running for.
    optionFind :: !Hook.FindBy,
    -- | "Client" configuration.
    optionClient :: !Client.Config,
    -- | Maximum number of scripts to run concurrently.
    optionMaxScripts :: !Natural,
    -- | The name of the script to run.
    optionScript :: !String,
    -- | Optional arguments to pass to the script.
    optionArgs :: ![String]
  }

-- | Command line option parser.
parser :: OA.Parser Options
parser =
  Options
    <$> Hook.parseFindBy
    <*> Client.configParser
    <*> OA.option
      OA.auto
      ( mconcat
          [ OA.long "max-scripts",
            OA.short 'm',
            OA.metavar "NUM",
            OA.help "Limit current script executions to NUM",
            OA.value 5
          ]
      )
    <*> OA.strArgument
      ( mconcat
          [ OA.metavar "script",
            OA.help "The script to run when the hook fires"
          ]
      )
    <*> many
      ( OA.strArgument $
          mconcat
            [ OA.metavar "arg",
              OA.help "Arguments to pass to the given script"
            ]
      )

-- | Main entry point for the @client@ command.
run ::
  forall m.
  MonadIO m =>
  MonadReader Env m =>
  MonadLog m =>
  Options ->
  m ()
run Options {..} = do
  env <- ask
  Database.runQuery (Env.database env) (Hook.findBy optionFind) >>= \case
    [] -> logInfo "no matching hooks found"
    hs -> liftIO $ do
      queue <- STM.newTBQueueIO optionMaxScripts
      mapConcurrently_ (go env queue) hs
  where
    go :: Env -> STM.TBQueue ByteString -> Hook.Hook -> IO ()
    go env queue hook = runLoggerIO (Env.loggingH env) $ do
      logInfo ("listening for hook " <> Hook.hookCode hook)
      readers <-
        liftIO $
          replicateM
            (fromIntegral optionMaxScripts)
            (async $ exec env hook queue)
      writer <-
        liftIO $
          async $
            Client.dispatch
              env
              (receive env hook queue)
              optionClient
              (Hook.hookAction hook)
      s <- liftIO (Async.wait writer)
      mapM_ (liftIO . Async.cancel) readers
      case s of
        Action.Okay ->
          pass
        Action.Invalid msg ->
          logError
            ( "an error occurred in hook "
                <> Hook.hookCode hook
                <> ": "
                <> toText msg
            )
        Action.Fail msg ->
          logError
            ( "action failed while running hook "
                <> Hook.hookCode hook
                <> ": "
                <> toText msg
            )

    -- Execute the script because the hook fired.
    receive :: Env -> Hook.Hook -> STM.TBQueue ByteString -> ByteString -> IO ()
    receive env hook queue bs = runLoggerIO (Env.loggingH env) $ do
      logInfo ("hook " <> Hook.hookCode hook <> " fired")
      atomically (STM.writeTBQueue queue bs)

    -- A thread for running the specified script when input is
    -- available.
    exec :: Env -> Hook.Hook -> STM.TBQueue ByteString -> IO ()
    exec env hook queue = runLoggerIO (Env.loggingH env) $
      forever $ do
        bs <- atomically (STM.readTBQueue queue)
        logDebug ("running script for hook " <> Hook.hookCode hook)
        liftIO (bracket create (snd >>> wait env) (fst >>> send bs))

    -- Create a process for the configured script.
    create :: IO (Handle, Process.ProcessHandle)
    create = do
      (pstdin, _, _, process) <-
        Process.proc optionScript optionArgs
          & (\c -> c {Process.std_in = Process.CreatePipe})
          & Process.createProcess
      case pstdin of
        Nothing -> throwString "impossible"
        Just h -> pure (h, process)

    -- Wait for the created process to exit.
    wait :: Env -> Process.ProcessHandle -> IO ()
    wait env process = do
      status <- Process.waitForProcess process
      runLoggerIO (Env.loggingH env) $
        case status of
          ExitSuccess ->
            logDebug
              ( "process '"
                  <> toText optionScript
                  <> "' finished successfully"
              )
          ExitFailure n ->
            logError
              ( "process '"
                  <> toText optionScript
                  <> "' exited with error code "
                  <> show n
              )

    -- Send the running process the payload on its standard input then
    -- close the handle to signal all input was sent.
    send :: ByteString -> Handle -> IO ()
    send bs h = do
      ByteString.hPut h bs
      hClose h
