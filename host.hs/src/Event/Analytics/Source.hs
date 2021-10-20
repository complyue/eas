{-# LANGUAGE MultiParamTypeClasses #-}

module Event.Analytics.Source where

-- import Debug.Trace

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Dynamic
import Data.IORef
import Data.Typeable hiding (TypeRep, typeOf, typeRep)
import Event.Analytics.Monad
import Language.Edh.MHI
import Type.Reflection
import Prelude

-- * Generic Event Source & Event Handling

-- | Indicates whether the relevant event sink is at end-of-stream
type EndOfStream = Bool

-- | An event handler reacts to a particular event
--
-- It should return whether it is still interested in the subsequent events
-- in the same (event source) stream
--
-- Further EIO computations can be scheduled into the provided atomic event
-- queue. Any failure in any event handler will prevent the publishing of the
-- current event hierarchy at all, the failure will be thrown to the publisher
-- of the root event.
type EventHandler t = t -> EAS HandleSubsequentEvents

data HandleSubsequentEvents = KeepHandling | StopHandling

class EventSource s t where
  -- | Obtain the lingering event data if any
  --
  -- Some event source can always have the latest event data lingering, some
  -- never, some per specific criteria.
  lingering :: s t -> EIO (Maybe t)

  -- | Handle each event data as it arrives
  perceive :: s t -> EventHandler t -> EIO ()

  -- | Subscribe to the event stream through the specified event source
  --
  -- Any failure in the handler will prevent publishing of the original event at
  -- all, such a failure will be thrown at the publisher of the original event.
  --
  -- The atomic event queue can be used to schedule EIO computations as
  -- consequences or subsequences of the original event.
  on :: s t -> (t -> EAS ()) -> EIO ()
  on !evs !handler = perceive evs $ \ !evd -> do
    handler evd
    return KeepHandling

  -- | Subscribe to the next event through the specified event source
  --
  -- Any failure in the handler will prevent publishing of the original event at
  -- all, such a failure will be thrown at the publisher of the original event.
  --
  -- The atomic event queue can be used to schedule EIO computations as
  -- consequences or subsequences of the original event.
  once :: s t -> (t -> EAS ()) -> EIO ()
  once !evs !handler = perceive evs $ \ !evd -> do
    handler evd
    return StopHandling

-- ** SomeEventSource the Functor

data MappedEvs s a b = (EventSource s a) => MappedEvs (s a) (a -> EIO b)

instance (EventSource s a) => EventSource (MappedEvs s a) b where
  lingering (MappedEvs sa f) =
    lingering sa >>= \case
      Nothing -> return Nothing
      Just a -> Just <$> f a
  perceive (MappedEvs sa f) handler = perceive sa $ handler <=< easDoEIO . f

-- | Polymorphic event source value wrapper
data SomeEventSource t = forall s. (EventSource s t) => SomeEventSource (s t)

instance Functor SomeEventSource where
  fmap f (SomeEventSource evs) = SomeEventSource $ MappedEvs evs $ return . f

-- ** EventSource Argument Adapters

-- | Monomorphic event source argument adapter
data EventSourceOf t
  = forall s. (EventSource s t, Typeable t) => EventSourceOf (s t) Object

instance Eq (EventSourceOf t) where
  EventSourceOf _ xo == EventSourceOf _ yo = xo == yo

instance Typeable t => ComputArgAdapter (EventSourceOf t) where
  adaptEdhArg !v = case edhUltimate v of
    EdhObject o -> do
      let go :: [Object] -> Edh (EventSourceOf t)
          go [] = mzero
          go (inst : rest) = (<|> go rest) $
            asEventSource inst $ \(evs :: s t') -> case eqT of
              Just (Refl :: t' :~: t) -> return $ EventSourceOf evs o
              Nothing -> go rest
      go . (o :) =<< readTVarEdh (edh'obj'supers o)
    _ -> mzero

  adaptedArgValue (EventSourceOf _evs !obj) = EdhObject obj

-- | Polymorphic event source argument adapter
data AnyEventSource
  = forall s t. (EventSource s t, Typeable t) => AnyEventSource (s t) Object

instance Eq AnyEventSource where
  AnyEventSource _ xo == AnyEventSource _ yo = xo == yo

instance ComputArgAdapter AnyEventSource where
  adaptEdhArg !v = case edhUltimate v of
    EdhObject o -> do
      let go :: [Object] -> Edh AnyEventSource
          go [] = mzero
          go (inst : rest) = (<|> go rest) $
            asEventSource inst $ \evs -> return $ AnyEventSource evs o
      go . (o :) =<< readTVarEdh (edh'obj'supers o)
    _ -> mzero

  adaptedArgValue (AnyEventSource _evs !obj) = EdhObject obj

asEventSource ::
  forall r.
  Object ->
  (forall s t. (EventSource s t, Typeable t) => s t -> Edh r) ->
  Edh r
asEventSource o withEvs = case dynamicHostData o of
  Nothing -> mzero
  Just (Dynamic tr evs) -> case tr `eqTypeRep` typeRep @SomeEventSink of
    Just HRefl -> case evs of
      SomeEventSink evs' ->
        withTypeable tr $ withEvs evs'
    _ -> case tr of
      App trEvs trE -> case trEvs `eqTypeRep` typeRep @SomeEventSource of
        Just HRefl -> case evs of
          SomeEventSource evs' ->
            withTypeable trE $ withEvs evs'
        _ -> case trEvs `eqTypeRep` typeRep @EventSink of
          Just HRefl -> withTypeable trE $ withEvs evs
          _ -> mzero
      _ -> mzero

-- * Event Sink - Event Target as well as Event Source

-- | An event sink conveys an event stream, with possibly multiple event
-- producers and / or multiple event consumers
data EventSink t = EventSink
  { -- | Subscribed event listeners to this sink
    event'sink'subscribers :: IORef [EventListener t],
    -- | The most recent event data lingering in this sink
    event'sink'recent :: IORef (Maybe t),
    -- | Whether this sink is at end-of-stream
    event'sink'eos :: IORef EndOfStream
  }

-- | An event listener reacts to a particular event
--
-- It should return a possible event listener for the next event in the stream
newtype EventListener t = EventListener
  {on'event :: t -> EAS (Maybe (EventListener t))}

-- | Subscribe an event handler to the event sink
handleEvents :: forall t. Typeable t => EventSink t -> EventHandler t -> EIO ()
handleEvents !evs !handler =
  liftIO $
    readIORef eosRef >>= \case
      True -> return ()
      False -> modifyIORef' subsRef (keepTriggering :)
  where
    eosRef = event'sink'eos evs
    subsRef = event'sink'subscribers evs

    keepTriggering :: EventListener t
    keepTriggering = EventListener $ \ !evd ->
      handler evd >>= \case
        KeepHandling -> return (Just keepTriggering)
        StopHandling -> return Nothing

-- ** SomeEventSink the Polymorphic Wrapper

-- | Polymorphic event sink value wrapper
data SomeEventSink = forall t. Typeable t => SomeEventSink (EventSink t)

instance Eq SomeEventSink where
  SomeEventSink x == SomeEventSink y = isSameEventSink x y

-- ** EventSink Argument Adapters

-- | Monomorphic event sink argument adapter
data EventSinkOf t = (Typeable t) => EventSinkOf (EventSink t) Object

instance Eq (EventSinkOf t) where
  EventSinkOf _ xo == EventSinkOf _ yo = xo == yo

instance Typeable t => ComputArgAdapter (EventSinkOf t) where
  adaptEdhArg !v = case edhUltimate v of
    EdhObject o -> do
      let go :: [Object] -> Edh (EventSinkOf t)
          go [] = mzero
          go (inst : rest) = (<|> go rest) $
            asEventSink inst $ \(evs :: EventSink t') -> case eqT of
              Just (Refl :: t' :~: t) -> return $ EventSinkOf evs o
              Nothing -> go rest
      go . (o :) =<< readTVarEdh (edh'obj'supers o)
    _ -> mzero

  adaptedArgValue (EventSinkOf _evs !obj) = EdhObject obj

-- | Polymorphic event sink argument adapter
data AnyEventSink = forall t. Typeable t => AnyEventSink (EventSink t) Object

instance Eq AnyEventSink where
  AnyEventSink x _ == AnyEventSink y _ = isSameEventSink x y

instance ComputArgAdapter AnyEventSink where
  adaptEdhArg !v = case edhUltimate v of
    EdhObject o -> do
      let go :: [Object] -> Edh AnyEventSink
          go [] = mzero
          go (inst : rest) = (<|> go rest) $
            asEventSink inst $ \evs -> return $ AnyEventSink evs o
      go . (o :) =<< readTVarEdh (edh'obj'supers o)
    _ -> mzero

  adaptedArgValue (AnyEventSink _evs !obj) = EdhObject obj

asEventSink ::
  forall r.
  Object ->
  (forall t. (Typeable t) => EventSink t -> Edh r) ->
  Edh r
asEventSink o withEvs = case dynamicHostData o of
  Nothing -> mzero
  Just (Dynamic tr evs) -> case tr `eqTypeRep` typeRep @SomeEventSink of
    Just HRefl -> case evs of
      SomeEventSink evs' -> withTypeable tr $ withEvs evs'
    _ -> case tr of
      App trEvs trE -> case trEvs `eqTypeRep` typeRep @EventSink of
        Just HRefl -> withTypeable trE $ withEvs evs
        _ -> mzero
      _ -> mzero

-- ** Utilities & Implementation Details

-- | Create a new event sink
newEventSinkEdh :: forall t. Edh (EventSink t)
newEventSinkEdh = liftIO newEventSinkIO

-- | Create a new event sink with specified data lingering
newEventSinkEdh' :: forall t. t -> Edh (EventSink t)
newEventSinkEdh' = liftIO . newEventSinkIO'

-- | Create a new event sink
newEventSinkEIO :: forall t. EIO (EventSink t)
newEventSinkEIO = liftIO newEventSinkIO

-- | Create a new event sink with specified data lingering
newEventSinkEIO' :: forall t. t -> EIO (EventSink t)
newEventSinkEIO' = liftIO . newEventSinkIO'

-- | Create a new event sink
newEventSinkIO :: forall t. IO (EventSink t)
newEventSinkIO = do
  !eosRef <- newIORef False
  !rcntRef <- newIORef Nothing
  !subsRef <- newIORef []
  return $ EventSink subsRef rcntRef eosRef

-- | Create a new event sink with specified data lingering
newEventSinkIO' :: forall t. t -> IO (EventSink t)
newEventSinkIO' d = do
  !eosRef <- newIORef False
  !rcntRef <- newIORef $ Just d
  !subsRef <- newIORef []
  return $ EventSink subsRef rcntRef eosRef

-- | Create a new event sink
newEventSink :: forall t. EAS (EventSink t)
newEventSink = easDoEIO newEventSinkEIO

-- | Create a new event sink with specified data lingering
newEventSink' :: forall t. t -> EAS (EventSink t)
newEventSink' = easDoEIO . newEventSinkEIO'

isSameEventSink :: forall a b. EventSink a -> EventSink b -> Bool
isSameEventSink a b = event'sink'eos a == event'sink'eos b

-- | Note identity of event sinks won't change after fmap'ped
instance Eq (EventSink a) where
  (==) = isSameEventSink

instance (Typeable t) => EventSource EventSink t where
  lingering = readIORefEIO . event'sink'recent
  perceive = handleEvents

-- * Event Propagation

-- | Spread one event data into the specified sink, as into current event frame
--
-- This can be called anytime from anywhere, event listeners/handlers are
-- triggered immediately, but consequences/subsequences of the spread event,
-- as spat from associated listeners/handlers, will only get realized during
-- 'driveEventFrame' called by simulation driver.
spreadEvent :: forall t. Typeable t => EventSink t -> t -> EAS ()
spreadEvent !evs !evd = do
  let spread ::
        [EventListener t] ->
        [EventListener t] ->
        EAS [EventListener t]
      spread subsRemain [] =
        return $! reverse subsRemain -- keep original order
      spread subsRemain (listener : rest) =
        on'event listener evd >>= \case
          Nothing -> spread subsRemain rest
          Just listener' -> spread (listener' : subsRemain) rest
  readIORefEAS eosRef >>= \case
    True -> return ()
    False -> do
      writeIORefEAS rcntRef $ Just evd
      readIORefEAS subsRef >>= spread [] >>= writeIORefEAS subsRef
  where
    eosRef = event'sink'eos evs
    rcntRef = event'sink'recent evs
    subsRef = event'sink'subscribers evs
