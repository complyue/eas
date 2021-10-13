module Event.Analytics.Monad where

-- import Debug.Trace
-- import GHC.Stack

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.State.Strict
import Data.IORef
import Data.Maybe
import Data.Unique
import Language.Edh.MHI
import Prelude

-- * Monadic Event Analytics & Simulation Interface

-- | Event Frame Queue
--
-- Foster the propagation of event hierarchies, on a frame by frame basis.
--
-- Computations can be scheduled into such a queue as either consequences or
-- subsequences, of the realization of a frame of whole event hierarchy, by
-- multiple events spread within such a frame, either directly or indirectly.
data EFQ = EFQ
  { efq'conseqs :: !(IORef [EAS ()]),
    efq'subseqs :: !(IORef [EAS ()])
  }

-- | Monad for Event Analytics & Simulation
--
-- Conceptually (i.e. not strictly technically), this is atop 'EIO'
newtype EAS a = EAS
  { unEAS ::
      EFQ ->
      EdhThreadState ->
      ([(ErrMessage, ErrContext)] -> IO ()) ->
      (a -> IO ()) ->
      IO ()
  }

instance MonadEdh EAS where
  liftEdh = easDoEIO . liftEdh

  edhThreadState = EAS $ \_efq ets _naExit exit -> exit ets

  throwEdhM' tag msg details = easDoEIO $ throwEdhM' tag msg details

  liftSTM = liftIO . atomically

easDoEIO :: forall a. EIO a -> EAS a
easDoEIO act = EAS $ \_efq ets _naExit exit -> runEIO act ets exit
{-# INLINE easDoEIO #-}

easNA :: forall a. ErrMessage -> EAS a
easNA msg = EAS $ \_efq ets naExit _exit -> naExit [(msg, getEdhErrCtx 0 ets)]
{-# INLINE easNA #-}

edhRunEAS :: EAS a -> Edh a
edhRunEAS = liftEIO . runEAS
{-# INLINE edhRunEAS #-}

runEAS :: EAS a -> EIO a
runEAS act = do
  cons <- newIORefEIO []
  subs <- newIORefEIO []
  runEAS' (EFQ cons subs) act
{-# INLINE runEAS #-}

runEAS' :: EFQ -> EAS a -> EIO a
runEAS' efq act = EIO $ \ets exit ->
  unEAS act efq ets (atomically . rptEdhNotApplicable) exit
{-# INLINE runEAS' #-}

instance Functor EAS where
  fmap f c = EAS $ \efq ets naExit exit -> unEAS c efq ets naExit $ exit . f
  {-# INLINE fmap #-}

instance Applicative EAS where
  pure a = EAS $ \_efq _ets _naExit exit -> exit a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad EAS where
  return = pure
  {-# INLINE return #-}
  m >>= k = EAS $ \efq ets naExit exit ->
    unEAS m efq ets naExit $ \a -> unEAS (k a) efq ets naExit exit
  {-# INLINE (>>=) #-}

instance Alternative EAS where
  empty = EAS $ \_efq _ets naExit _exit -> naExit []
  x <|> y = EAS $ \efq ets naExit exit ->
    unEAS
      x
      efq
      ets
      (\errOut -> unEAS y efq ets (naExit . (++ errOut)) exit)
      exit

instance MonadPlus EAS

instance MonadIO EAS where
  liftIO act = EAS $ \_efq _ets _naExit exit -> act >>= exit
  {-# INLINE liftIO #-}

-- ** EAS Monad Utilities

-- | Schedule a consequent action of the event frame propagation
conseqDo :: EAS () -> EAS ()
conseqDo act = EAS $ \(EFQ !conseqs _) _ets _naExit exit -> do
  modifyIORef' conseqs (act :)
  exit ()
{-# INLINE conseqDo #-}

-- | Schedule a subsequent action of the event frame propagation
subseqDo :: EAS () -> EAS ()
subseqDo act = EAS $ \(EFQ _ !subseqs) _ets _naExit exit -> do
  modifyIORef' subseqs (act :)
  exit ()
{-# INLINE subseqDo #-}

-- Usually called by a simulation driver, to drain the event frame queue, and
-- so as to realize all consequences/subsequences of all events spread during
-- last frame iteration, including those events spread by the simulation
-- driver.
--
-- Consequent actions will see all event sinks so updated (including but not
-- limited to, lingering recent event data), by events spread in last iteration
-- of the same frame.
--
-- Subsequent actions will see all effects applied by consequent actions,
-- new events spread in subsequent actions will trigger listeners/handlers
-- immediately, so new consequeces/subsequences of them will be applied in
-- the next iteration, although also in current event frame.
--
-- An event frame is only done when no new events are spread in the last
-- iteration, then this function will return, giving control back to the
-- simulation driver.
driveEventFrame :: EAS ()
driveEventFrame = do
  EFQ !conseqs !subseqs <- easQueue
  let driveFrame =
        drain conseqs >>= \case
          True -> return () -- empty queue, frame done
          False -> do
            void $ drain subseqs
            -- subsequent actions may spread more events, check that by
            -- attempting yet another iteration
            driveFrame
  driveFrame
  where
    drain :: IORef [EAS ()] -> EAS Bool
    drain q =
      readIORefEAS q >>= \case
        [] -> return True
        acts -> do
          writeIORefEAS q []
          propagate acts
          void $ drain q
          return False
    propagate :: [EAS ()] -> EAS ()
    propagate [] = return ()
    propagate (act : rest) = do
      act -- TODO catch any tolerable exception, and keep going
      propagate rest

-- | Obtain current event queue
easQueue :: EAS EFQ
easQueue = EAS $ \efq _ets _naExit exit -> exit efq
{-# INLINE easQueue #-}

-- | Obtain current event queue
easThreadState :: EAS EdhThreadState
easThreadState = EAS $ \_efq ets _naExit exit -> exit ets
{-# INLINE easThreadState #-}

-- | Shorthand for @'liftIO' . 'atomically'@
atomicallyEAS :: STM a -> EAS a
atomicallyEAS = liftIO . atomically
{-# INLINE atomicallyEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
newTVarEAS :: forall a. a -> EAS (TVar a)
newTVarEAS = liftIO . newTVarIO
{-# INLINE newTVarEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
readTVarEAS :: forall a. TVar a -> EAS a
readTVarEAS = liftIO . readTVarIO
{-# INLINE readTVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
writeTVarEAS :: forall a. TVar a -> a -> EAS ()
writeTVarEAS ref v = atomicallyEAS $ writeTVar ref v
{-# INLINE writeTVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
modifyTVarEAS' :: forall a. TVar a -> (a -> a) -> EAS ()
modifyTVarEAS' ref f = atomicallyEAS $ modifyTVar' ref f
{-# INLINE modifyTVarEAS' #-}

-- | The 'STM' action lifted into 'EAS' monad
swapTVarEAS :: forall a. TVar a -> a -> EAS a
swapTVarEAS ref a = atomicallyEAS $ swapTVar ref a
{-# INLINE swapTVarEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
newTMVarEAS :: forall a. a -> EAS (TMVar a)
newTMVarEAS = liftIO . newTMVarIO
{-# INLINE newTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
readTMVarEAS :: forall a. TMVar a -> EAS a
readTMVarEAS = atomicallyEAS . readTMVar
{-# INLINE readTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
takeTMVarEAS :: forall a. TMVar a -> EAS a
takeTMVarEAS = atomicallyEAS . takeTMVar
{-# INLINE takeTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
putTMVarEAS :: forall a. TMVar a -> a -> EAS ()
putTMVarEAS ref v = atomicallyEAS $ putTMVar ref v
{-# INLINE putTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
tryReadTMVarEAS :: forall a. TMVar a -> EAS (Maybe a)
tryReadTMVarEAS = atomicallyEAS . tryReadTMVar
{-# INLINE tryReadTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
tryTakeTMVarEAS :: forall a. TMVar a -> EAS (Maybe a)
tryTakeTMVarEAS = atomicallyEAS . tryTakeTMVar
{-# INLINE tryTakeTMVarEAS #-}

-- | The 'STM' action lifted into 'EAS' monad
tryPutTMVarEAS :: forall a. TMVar a -> a -> EAS Bool
tryPutTMVarEAS ref v = atomicallyEAS $ tryPutTMVar ref v
{-# INLINE tryPutTMVarEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
newIORefEAS :: forall a. a -> EAS (IORef a)
newIORefEAS = liftIO . newIORef
{-# INLINE newIORefEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
readIORefEAS :: forall a. IORef a -> EAS a
readIORefEAS = liftIO . readIORef
{-# INLINE readIORefEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
writeIORefEAS :: forall a. IORef a -> a -> EAS ()
writeIORefEAS ref v = liftIO $ writeIORef ref v
{-# INLINE writeIORefEAS #-}

-- | The 'IO' action lifted into 'EAS' monad
newUniqueEAS :: EAS Unique
newUniqueEAS = liftIO newUnique
{-# INLINE newUniqueEAS #-}

-- ** Exceptions with EAS

-- | Throw a general exception from an 'EAS' action
throwHostEAS :: Exception e => e -> EAS a
throwHostEAS = liftIO . throwIO
{-# INLINE throwHostEAS #-}
