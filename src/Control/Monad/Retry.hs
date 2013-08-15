-- |
-- Module     : Control.Monad.Retry
-- Copyright  : (c) 2013 Toralf Wittner
-- License    : MPL-2.0
-- Maintainer : Toralf Wittner <tw@dtex.org>
--
-- This module is similar to @retry-0.3.0.0@ by Ozgun Ataman and based
-- on its idea. It differs mostly in implementation details, e.g.
--
--      * Depends on @exceptions@ instead on @monad-control@.
--
--      * Catches all but a few (mostly asynchronous) exceptions instead of
--        allowing clients to provide exception 'Handler's.
--
module Control.Monad.Retry
    ( Settings (..)
    , Limit    (..)
    , retry
    , recover
    , redo
    ) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
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
--
-- /Please note that asynchronous exceptions from/ @base@ /are not catched./
recover :: (MonadIO m, MonadCatch m) => Settings -> m a -> m a
recover s a = run 0
  where
    run n = runEitherT (catchSome a) >>= continue n

    continue n (Left e)  = case retries s of
        Unlimited -> next n
        Limited k -> if n >= k then throwM e else next n
    continue _ (Right x) = return x

    next n = liftIO (delay s n) >> run (n + 1)

-- | The combination of 'retry' and 'recover':
--
-- @redo s stop a = retry s stop (recover s a)@
--
-- /Note that the total number of executions may thus be n * (n - 1)/.
redo :: (MonadIO m, MonadCatch m)
     => Settings
     -> (a -> Bool) -- ^ Termination check: @True@ = finish, @False@ = retry.
     -> m a         -- ^ Retryable action.
     -> m a
redo s stop a = retry s stop (recover s a)

-- Internals:

catchSome :: MonadCatch m => m a -> EitherT SomeException m a
catchSome a = EitherT $ catches (a >>= return . Right)
    [ Handler $ \e -> throwM (e :: E.ArithException)
    , Handler $ \e -> throwM (e :: E.ArrayException)
    , Handler $ \e -> throwM (e :: E.AssertionFailed)
    , Handler $ \e -> throwM (e :: E.AsyncException)
    , Handler $ \e -> throwM (e :: E.BlockedIndefinitelyOnMVar)
    , Handler $ \e -> throwM (e :: E.BlockedIndefinitelyOnSTM)
    , Handler $ \e -> throwM (e :: E.Deadlock)
    , Handler $ \e -> throwM (e ::   Dynamic)
    , Handler $ \e -> throwM (e :: E.ErrorCall)
    , Handler $ \e -> throwM (e ::   ExitCode)
    , Handler $ \e -> throwM (e :: E.NestedAtomically)
    , Handler $ \e -> throwM (e :: E.NoMethodError)
    , Handler $ \e -> throwM (e :: E.NonTermination)
    , Handler $ \e -> throwM (e :: E.PatternMatchFail)
    , Handler $ \e -> throwM (e :: E.RecConError)
    , Handler $ \e -> throwM (e :: E.RecSelError)
    , Handler $ \e -> throwM (e :: E.RecUpdError)
    , Handler $ return . Left
    ]

delay :: Settings -> Word -> IO ()
delay s n = threadDelay . fromIntegral $
    initDelay s * (if backoff s then 2 ^ n else 1)
