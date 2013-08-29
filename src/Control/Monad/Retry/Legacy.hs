{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module     : Control.Monad.Retry.Legacy
-- Copyright  : (c) 2013 Toralf Wittner
-- License    : MPL-2.0
-- Maintainer : Toralf Wittner <tw@dtex.org>
--
-- This module is similar to @retry-0.3.0.0@ by Ozgun Ataman and based
-- on its idea. It differs mostly in implementation details and depends
-- on @MonadCatchIO-transformers@ instead on @monad-control@.

module Control.Monad.Retry.Legacy
    ( Settings (..)
    , Limit    (..)
    , retry
    , recover
    , redo
    , redo_
    ) where

import Control.Concurrent
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Data.Dynamic (Dynamic)
import Data.Word
import System.Exit (ExitCode)
import qualified Control.Exception as E

data Limit
    = Limited Word -- ^ Number of retries.
    | Unlimited
    deriving (Eq, Show)

data Settings = Settings
    { retries   :: Limit
    , backoff   :: Bool  -- ^ Use exponential backoff?
    , initDelay :: Word  -- ^ Initial delay in micro-seconds.
    } deriving (Eq, Show)

-- | Retry a given monadic action.
retry :: MonadIO m
      => Settings
      -> (a -> Bool) -- ^ Termination check: @True@ = finish, @False@ = retry.
      -> m a         -- ^ Retryable action.
      -> m a
retry s stop a = run 0
  where
    run n = a >>= continue n

    continue n x
        | stop x    = return x
        | otherwise = case retries s of
            Unlimited -> next n
            Limited k -> if n >= k then return x else next n

    next n = liftIO (delay s n) >> run (n + 1)

-- | Turn the given monadic action into one that is retried if an
-- exception occurs.
recover :: (MonadIO m, MonadCatchIO m)
        => Settings
        -> [Handler m Bool] -- ^ Exception handlers.
        -> m a              -- ^ Retryable action.
        -> m a
recover s h a = run 0
  where
    run n = catches a (map (handler n) h)

    handler n (Handler f) = Handler $ \e -> f e >>= \b ->
        if b
            then case retries s of
                Unlimited -> next n
                Limited k -> if n >= k then throw e else next n
            else throw e

    next n = liftIO (delay s n) >> run (n + 1)

-- | The combination of 'retry' and 'recover':
--
-- @redo s stop h a = retry s stop (recover s h a)@
--
-- /Note that the total number of executions may thus be n * (n - 1)/.
redo :: (MonadIO m, MonadCatchIO m)
     => Settings
     -> (a -> Bool)      -- ^ Termination check: @True@ = finish, @False@ = retry.
     -> [Handler m Bool] -- ^ Exception handlers.
     -> m a              -- ^ Retryable action.
     -> m a
redo s stop hdlr a = retry s stop (recover s hdlr a)

-- | The combination of 'retry' and 'recover' ignoring most exceptions from
-- @base@.
redo_ :: (MonadIO m, MonadCatchIO m)
      => Settings
      -> (a -> Bool)      -- ^ Termination check: @True@ = finish, @False@ = retry.
      -> m a              -- ^ Retryable action.
      -> m a
redo_ s stop a = retry s stop (recover s allExceptBase a)

-- Internal:

allExceptBase :: (MonadIO m, MonadCatchIO m) => [Handler m Bool]
allExceptBase =
    [ Handler $ \(e :: E.ArithException) -> throw e
    , Handler $ \(e :: E.ArrayException) -> throw e
    , Handler $ \(e :: E.AssertionFailed) -> throw e
    , Handler $ \(e :: E.AsyncException) -> throw e
    , Handler $ \(e :: E.BlockedIndefinitelyOnMVar) -> throw e
    , Handler $ \(e :: E.BlockedIndefinitelyOnSTM) -> throw e
    , Handler $ \(e :: E.Deadlock) -> throw e
    , Handler $ \(e :: Dynamic) -> throw e
    , Handler $ \(e :: E.ErrorCall) -> throw e
    , Handler $ \(e :: ExitCode) -> throw e
    , Handler $ \(e :: E.NestedAtomically) -> throw e
    , Handler $ \(e :: E.NoMethodError) -> throw e
    , Handler $ \(e :: E.NonTermination) -> throw e
    , Handler $ \(e :: E.PatternMatchFail) -> throw e
    , Handler $ \(e :: E.RecConError) -> throw e
    , Handler $ \(e :: E.RecSelError) -> throw e
    , Handler $ \(e :: E.RecUpdError) -> throw e
    , Handler $ \(_ :: E.SomeException) -> return True
    ]

delay :: Settings -> Word -> IO ()
delay s n = threadDelay . fromIntegral $
    initDelay s * (if backoff s then 2 ^ n else 1)
