module Event.Analytics.EvsArts where

-- import           Debug.Trace

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Maybe
import qualified Data.Text as T
import Event.Analytics.DataType
import Event.Analytics.EvsDtArts
import Event.Analytics.Source
import Language.Edh.EHI
import Type.Reflection
import Prelude

defineEvsArts :: Edh ()
defineEvsArts = exportM_ $ do
  clsEventSource <- defEventSourceClass

  dtYesNo <- defYesNoEvtDt clsEventSource "YesNo"
  dtDouble <- defFloatEvtDt @Double clsEventSource dtYesNo "Double"

  defSinkClass dtDouble

defEventSourceClass :: Edh Object
defEventSourceClass =
  defEdhClass' "EventSource" evtAllocator [] $ do
    defEdhProc'_ EdhMethod "__repr__" evtReprProc
    defEdhProperty_ "dtype" evsDtypeProc Nothing
  where
    evtAllocator :: ArgsPack -> Edh ObjectStore
    evtAllocator _ =
      throwEdhM UsageError "EventSource not constructable by script"

    evtReprProc :: Edh EdhValue
    evtReprProc = withThisEventSource $ \this (_evs :: s t) -> do
      !dto <- getEvsDtype this
      !dtRepr <- edhObjReprM dto
      let evtRepr = "EventSource( dtype= " <> dtRepr <> " )"
      return $ EdhString evtRepr

withThisEventSource ::
  forall r.
  (forall s t. (EventSource s t, Typeable t) => Object -> s t -> Edh r) ->
  Edh r
withThisEventSource withEvs = do
  !this <- edh'scope'this . contextScope . edh'context <$> edhThreadState
  (<|> throwEdhM EvalError "this is not an EventSource") $
    asEventSource this $ withEvs this

defSinkClass :: Object -> Edh Object
defSinkClass !defaultDt =
  defEdhClass' "Sink" evsAllocator [] $ do
    defEdhProc'_ EdhMethod "__init__" evs__init__
    defEdhProc'_ EdhMethod "__repr__" evsReprProc
    defEdhProc'_ EdhMethod "__show__" evsShowProc
    defEdhProperty_ "dtype" evsDtypeProc Nothing
  where
    evsAllocator ::
      "dtype" ?: Object ->
      ArgsPack -> -- allow/ignore arbitrary ctor args for descendant classes
      Edh ObjectStore
    evsAllocator (defaultArg defaultDt -> dto) _ctorOtherArgs =
      withDataType dto $ \case
        DeviceDt (_dt :: DeviceDataType a) -> do
          evs <- newEventSinkEdh @a
          pinAndStoreHostValue $ SomeEventSink evs
        DirectDt dt -> case direct'data'default dt of
          (_fill'val :: a) -> do
            evs <- newEventSinkEdh @a
            pinAndStoreHostValue $ SomeEventSink evs
        (DummyDt _dti :: DataType a) -> do
          evs <- newEventSinkEdh @a
          pinAndStoreHostValue $ SomeEventSink evs

    evs__init__ ::
      "dtype" ?: Object ->
      ArgsPack -> -- allow/ignore arbitrary ctor args for descendant classes
      Edh EdhValue
    evs__init__ (defaultArg defaultDt -> dto) _ctorOtherArgs = do
      ets <- edhThreadState
      let scope = contextScope $ edh'context ets
          this = edh'scope'this scope
          that = edh'scope'that scope

          extendsDt :: [Object] -> Edh ()
          extendsDt [] = return ()
          extendsDt (o : rest) = do
            modifyTVarEdh' (edh'obj'supers o) (++ [dto])
            if o == this
              then return ()
              else extendsDt rest

      supers <- readTVarEdh $ edh'obj'supers that
      extendsDt $ that : supers
      return nil

    withThisSink ::
      forall r.
      (forall a. Typeable a => Object -> EventSink a -> Edh r) ->
      Edh r
    withThisSink withEvs = do
      !this <- edh'scope'this . contextScope . edh'context <$> edhThreadState
      (<|> throwEdhM EvalError "bug: this is not a Sink") $
        case objDynamicValue this of
          Nothing -> naM "not a Sink"
          Just (Dynamic trEvs evs) ->
            case trEvs `eqTypeRep` typeRep @SomeEventSink of
              Just HRefl -> case evs of
                SomeEventSink evs' -> withEvs this evs'
              Nothing -> case trEvs of
                App trSink trEvt ->
                  case trSink `eqTypeRep` typeRep @EventSink of
                    Just HRefl -> withTypeable trEvt $ withEvs this evs
                    Nothing -> naM "not a Sink"
                _ -> naM "not a Sink"

    evsReprProc :: Edh EdhValue
    evsReprProc = withThisSink $ \this (_evs :: EventSink a) -> do
      !dto <- getEvsDtype this
      !dtRepr <- edhObjReprM dto
      let evsRepr = "Sink( dtype= " <> dtRepr <> " )"
      return $ EdhString evsRepr

    evsShowProc :: Edh EdhValue
    evsShowProc = withThisSink $ \this (_evs :: EventSink a) -> do
      !dto <- getEvsDtype this
      !dtRepr <- edhObjReprM dto
      let evsRepr = "Sink( dtype= " <> dtRepr <> " )"
      return $
        EdhString $ evsRepr <> " {# " <> T.pack (show $ typeRep @a) <> " #}"

evsDtypeProc :: Edh EdhValue
evsDtypeProc = do
  !this <- edh'scope'this . contextScope . edh'context <$> edhThreadState
  EdhObject <$> getEvsDtype this
