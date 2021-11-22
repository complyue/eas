module Event.Analytics.EvsDtArts where

-- import           Debug.Trace

import Control.Applicative
import Control.Monad
import Data.Dynamic
import Data.Maybe
import qualified Data.Text as T
import Data.Typeable hiding (typeRep)
import Event.Analytics.DataType
import Event.Analytics.Source
import Event.Analytics.XCHG
import Foreign hiding (void)
import GHC.Float
import Language.Edh.EHI
import System.Random
import Type.Reflection
import Prelude

{- HLINT ignore "Redundant <$>" -}

withEvsOf ::
  forall t r.
  (Typeable t) =>
  Object ->
  (forall s. (EventSource s t) => Object -> s t -> Edh r) ->
  Edh r
withEvsOf obj withEvs = go . (obj :) =<< readTVarEdh (edh'obj'supers obj)
  where
    go :: [Object] -> Edh r
    go [] =
      naM $
        T.pack $
          "not an expected EventSource of event type: " <> show (typeRep @t)
    go (inst : rest) = (<|> go rest) $
      asEventSource inst $ \(evs :: s t') -> case eqT of
        Nothing ->
          naM $
            T.pack $
              "event type of the EventSource mismatches expection: "
                <> show (typeRep @t')
                <> " vs "
                <> show (typeRep @t)
        Just (Refl :: t' :~: t) -> withEvs inst evs

getEvsDtype :: Object -> Edh Object
getEvsDtype !objEvs = do
  let findEvsDto :: [Object] -> Edh Object
      findEvsDto [] =
        edhSimpleDescM (EdhObject objEvs) >>= \ !badDesc ->
          naM $ "not a Sink/EventSource os dtype: " <> badDesc
      -- this is right and avoids unnecessary checks in vastly usual cases
      findEvsDto [dto] = return dto
      -- safe guard in case an evs instance has been further extended
      findEvsDto (maybeDto : rest) =
        (<|> findEvsDto rest) $
          withDataType maybeDto $ const $ return maybeDto
  readTVarEdh (edh'obj'supers objEvs) >>= findEvsDto

defineEvtFieldProperty ::
  forall t s.
  (Typeable s, Typeable t) =>
  (s -> t) ->
  Object ->
  AttrName ->
  Edh EdhValue
defineEvtFieldProperty fg dto nm = do
  clsEvs <- getEventSourceClass
  let getter :: Edh EdhValue
      getter = do
        !that <- edh'scope'that . contextScope . edh'context <$> edhThreadState
        withEvsOf @s that $ \_selfEvsObj (selfEvs :: s' s) -> do
          let exitWithResult :: MappedEvs s' s t -> Edh EdhValue
              exitWithResult !evsResult =
                EdhObject
                  <$> createArbiHostObjectM'
                    clsEvs
                    (SomeEventSource evsResult)
                    [dto]
          exitWithResult $ MappedEvs selfEvs $ return . fg
  withDataType dto $
    \(_ :: DataType a) -> case eqT of
      Just (Refl :: a :~: t) -> mkEdhProperty nm getter Nothing
      Nothing ->
        throwEdhM UsageError $
          T.pack $
            "incompatible field type with dtype: " <> show (typeRep @t)
              <> " vs "
              <> show (typeRep @a)

defYesNoEvtDt :: Object -> DataTypeIdent -> Edh Object
defYesNoEvtDt clsEvs !dti = do
  dtCls <- mkEdhClass dti (allocObjM dtypeAllocator) [] $ do
    defEdhProc'_ EdhMethod "(&&)" $ evsOpProc @YesNo clsEvs (.&.)
    defEdhProc'_ EdhMethod "(&&.)" $ evsOpProc @YesNo clsEvs (.&.)
    defEdhProc'_ EdhMethod "(||)" $ evsOpProc @YesNo clsEvs (.|.)
    defEdhProc'_ EdhMethod "(||.)" $ evsOpProc @YesNo clsEvs (.|.)

    defEdhProc'_ EdhMethod "__eq__" evsDtypeEqProc
    defEdhArt "__repr__" $ EdhString dti
  !supersVar <- newTVarEdh []
  let dtYesNo =
        Object
          { edh'obj'store = dtd,
            edh'obj'class = dtCls,
            edh'obj'supers = supersVar
          }
  clsScope <- inlineSTM $ objectScope dtCls
  runNestedIn clsScope $ do
    defEdhProc'_ EdhMethod "(==)" $
      evsCmpProc clsEvs dtYesNo ((==) :: YesNo -> YesNo -> Bool)
    defEdhProc'_ EdhMethod "(==.)" $
      evsCmpProc clsEvs dtYesNo ((==) :: YesNo -> YesNo -> Bool)
    defEdhProc'_ EdhMethod "(!=)" $
      evsCmpProc clsEvs dtYesNo ((/=) :: YesNo -> YesNo -> Bool)
    defEdhProc'_ EdhMethod "(!=.)" $
      evsCmpProc clsEvs dtYesNo ((/=) :: YesNo -> YesNo -> Bool)
  defEdhArt dti $ EdhObject dtYesNo
  return dtYesNo
  where
    !dtd = HostStore $ wrapHostValue dt
    dt :: DataType YesNo
    dt = mkIntDataType @YesNo dti

    dtypeAllocator :: Edh ObjectStore
    dtypeAllocator = return dtd

defFloatEvtDt ::
  forall a.
  Object ->
  (RealFloat a, Random a, Num a, Storable a, EdhXchg a, Typeable a) =>
  Object ->
  DataTypeIdent ->
  Edh Object
defFloatEvtDt clsEvs !dtYesNo !dti = do
  !dtCls <- mkEdhClass dti (allocObjM dtypeAllocator) [] $ do
    defEdhProc'_ EdhMethod "(==)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(==.)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(!=)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(!=.)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(>=)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>=.)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=.)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(>.)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<.)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(+)" $ evsOpProc @a clsEvs (+)
    defEdhProc'_ EdhMethod "(+.)" $ evsOpProc @a clsEvs (+)
    defEdhProc'_ EdhMethod "(-)" $ evsOpProc @a clsEvs (-)
    defEdhProc'_ EdhMethod "(-.)" $ evsOpProc @a clsEvs (flip (-))
    defEdhProc'_ EdhMethod "(*)" $ evsOpProc @a clsEvs (*)
    defEdhProc'_ EdhMethod "(*.)" $ evsOpProc @a clsEvs (*)
    defEdhProc'_ EdhMethod "(/)" $ evsOpProc @a clsEvs (/)
    defEdhProc'_ EdhMethod "(/.)" $ evsOpProc @a clsEvs (flip (/))
    -- TODO reason about this:
    -- https://stackoverflow.com/questions/38588815/rounding-errors-in-python-floor-division
    defEdhProc'_ EdhMethod "(//)" $
      evsOpProc @a clsEvs (\ !x !y -> fromInteger $ floor $ x / y)
    defEdhProc'_ EdhMethod "(//.)" $
      evsOpProc @a clsEvs (\ !x !y -> fromInteger $ floor $ y / x)
    defEdhProc'_ EdhMethod "(**)" $ evsOpProc @a clsEvs (**)
    defEdhProc'_ EdhMethod "(**.)" $ evsOpProc @a clsEvs (flip (**))

    defEdhProc'_ EdhMethod "__eq__" evsDtypeEqProc
    defEdhArt "__repr__" $ EdhString dti

  !supersVar <- newTVarEdh []
  let !dtObj =
        Object
          { edh'obj'store = dtd,
            edh'obj'class = dtCls,
            edh'obj'supers = supersVar
          }
  defEdhArt dti $ EdhObject dtObj
  return dtObj
  where
    !dtd = HostStore $ wrapHostValue dt
    dt :: DataType a
    dt = mkFloatDataType @a dti

    dtypeAllocator :: Edh ObjectStore
    dtypeAllocator = return dtd

defIntEvtDt ::
  forall a.
  Object ->
  (Bits a, Integral a, Random a, Num a, Storable a, EdhXchg a, Typeable a) =>
  Object ->
  DataTypeIdent ->
  Edh Object
defIntEvtDt clsEvs !dtYesNo !dti = do
  !dtCls <- mkEdhClass dti (allocObjM dtypeAllocator) [] $ do
    defEdhProc'_ EdhMethod "(==)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(==.)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(!=)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(!=.)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(>=)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>=.)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=.)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(>.)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<.)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(+)" $ evsOpProc @a clsEvs (+)
    defEdhProc'_ EdhMethod "(+.)" $ evsOpProc @a clsEvs (+)
    defEdhProc'_ EdhMethod "(-)" $ evsOpProc @a clsEvs (-)
    defEdhProc'_ EdhMethod "(-.)" $ evsOpProc @a clsEvs (flip (-))
    defEdhProc'_ EdhMethod "(*)" $ evsOpProc @a clsEvs (*)
    defEdhProc'_ EdhMethod "(*.)" $ evsOpProc @a clsEvs (*)
    defEdhProc'_ EdhMethod "(/)" $ evsOpProc @a clsEvs div
    defEdhProc'_ EdhMethod "(/.)" $ evsOpProc @a clsEvs (flip div)
    defEdhProc'_ EdhMethod "(//)" $ evsOpProc @a clsEvs (\ !x !y -> div x y)
    defEdhProc'_ EdhMethod "(//.)" $ evsOpProc @a clsEvs (\ !x !y -> div y x)
    defEdhProc'_ EdhMethod "(**)" $ evsOpProc @a clsEvs intPow
    defEdhProc'_ EdhMethod "(**.)" $ evsOpProc @a clsEvs (flip intPow)
    defEdhProc'_ EdhMethod "(&&)" $ evsOpProc @a clsEvs (.&.)
    defEdhProc'_ EdhMethod "(&&.)" $ evsOpProc @a clsEvs (.&.)
    defEdhProc'_ EdhMethod "(||)" $ evsOpProc @a clsEvs (.|.)
    defEdhProc'_ EdhMethod "(||.)" $ evsOpProc @a clsEvs (.|.)

    defEdhProc'_ EdhMethod "__eq__" evsDtypeEqProc
    defEdhArt "__repr__" $ EdhString dti

  !supersVar <- newTVarEdh []
  let !dtObj =
        Object
          { edh'obj'store = dtd,
            edh'obj'class = dtCls,
            edh'obj'supers = supersVar
          }
  defEdhArt dti $ EdhObject dtObj
  return dtObj
  where
    !dtd = HostStore $ wrapHostValue dt
    dt :: DataType a
    dt = mkIntDataType @a dti

    dtypeAllocator :: Edh ObjectStore
    dtypeAllocator = return dtd

    intPow :: a -> a -> a
    intPow _ 0 = 1
    intPow x y
      -- TODO this justifies?
      | y < 0 = 0 -- to survive `Exception: Negative exponent`
      | otherwise = x ^ y

defBitsEvtDt ::
  forall a.
  Object ->
  (Bits a, Ord a, Storable a, EdhXchg a, Typeable a) =>
  Object ->
  DataTypeIdent ->
  Edh Object
defBitsEvtDt clsEvs !dtYesNo !dti = do
  !dtCls <- mkEdhClass dti (allocObjM dtypeAllocator) [] $ do
    defEdhProc'_ EdhMethod "(==)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(==.)" $ evsCmpProc @a clsEvs dtYesNo (==)
    defEdhProc'_ EdhMethod "(!=)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(!=.)" $ evsCmpProc @a clsEvs dtYesNo (/=)
    defEdhProc'_ EdhMethod "(>=)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>=.)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=)" $ evsCmpProc @a clsEvs dtYesNo (<=)
    defEdhProc'_ EdhMethod "(<=.)" $ evsCmpProc @a clsEvs dtYesNo (>=)
    defEdhProc'_ EdhMethod "(>)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(>.)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<)" $ evsCmpProc @a clsEvs dtYesNo (<)
    defEdhProc'_ EdhMethod "(<.)" $ evsCmpProc @a clsEvs dtYesNo (>)
    defEdhProc'_ EdhMethod "(&&)" $ evsOpProc @a clsEvs (.&.)
    defEdhProc'_ EdhMethod "(&&.)" $ evsOpProc @a clsEvs (.&.)
    defEdhProc'_ EdhMethod "(||)" $ evsOpProc @a clsEvs (.|.)
    defEdhProc'_ EdhMethod "(||.)" $ evsOpProc @a clsEvs (.|.)

    defEdhProc'_ EdhMethod "__eq__" evsDtypeEqProc
    defEdhArt "__repr__" $ EdhString dti

  !supersVar <- newTVarEdh []
  let !dtObj =
        Object
          { edh'obj'store = dtd,
            edh'obj'class = dtCls,
            edh'obj'supers = supersVar
          }
  defEdhArt dti $ EdhObject dtObj
  return dtObj
  where
    !dtd = HostStore $ wrapHostValue dt
    dt :: DataType a
    dt = mkBitsDataType @a dti

    dtypeAllocator :: Edh ObjectStore
    dtypeAllocator = return dtd

defEvsDataType ::
  forall a.
  (Typeable a) =>
  DataTypeIdent ->
  Edh () ->
  Edh Object
defEvsDataType !dti !clsInit = do
  !dtCls <- mkEdhClass dti (allocObjM dtypeAllocator) [] $ do
    defEdhArt "__repr__" $ EdhString dti
    clsInit
  !supersVar <- newTVarEdh []
  let !dtObj =
        Object
          { edh'obj'store = dtd,
            edh'obj'class = dtCls,
            edh'obj'supers = supersVar
          }
  defEdhArt dti $ EdhObject dtObj
  return dtObj
  where
    !dtd = HostStore $ wrapHostValue dt
    dt :: DataType a
    dt = DummyDt @a dti

    dtypeAllocator :: Edh ObjectStore
    dtypeAllocator = return dtd

evsCmpProc ::
  forall a.
  Object ->
  (Eq a, EdhXchg a, Typeable a) =>
  Object ->
  (a -> a -> Bool) ->
  EdhValue ->
  Edh EdhValue
evsCmpProc clsEvs !dtYesNo !cmp !other = do
  !that <- edh'scope'that . contextScope . edh'context <$> edhThreadState
  withEvsOf @a that $ \_selfEvsObj (selfEvs :: s a) -> do
    let exitWithResult :: MappedEvs s a YesNo -> Edh EdhValue
        exitWithResult !evsResult = do
          EdhObject
            <$> createArbiHostObjectM'
              clsEvs
              (SomeEventSource evsResult)
              [dtYesNo]

        withEvs =
          adaptEdhArg @AnyEventSource other
            >>= \(AnyEventSource (otherEvs :: s' t) _otherEvsObj) ->
              case eqT of
                Just (Refl :: t :~: a) -> exitWithResult $
                  MappedEvs selfEvs $ \ !selfEvd ->
                    lingering otherEvs >>= \case
                      Nothing -> return $ yesOrNo False
                      Just !rhv -> return $ yesOrNo $ cmp selfEvd rhv
                Nothing ->
                  throwEdhM UsageError $
                    T.pack $
                      "incompatible event data type: " <> show (typeRep @t)
                        <> " vs "
                        <> show (typeRep @a)

        withValue =
          fromEdh' @a other >>= \case
            Nothing -> return edhNA
            Just !rhv -> exitWithResult $
              MappedEvs selfEvs $ \ !selfEvd ->
                return $ yesOrNo $ cmp selfEvd rhv

    withEvs <|> withValue

evsOpProc ::
  forall a.
  Object ->
  (Eq a, EdhXchg a, Typeable a) =>
  (a -> a -> a) ->
  EdhValue ->
  Edh EdhValue
evsOpProc clsEvs !op !other = do
  !that <- edh'scope'that . contextScope . edh'context <$> edhThreadState
  withEvsOf @a that $ \ !selfEvsObj (selfEvs :: s a) -> do
    let exitWithResult :: MappedEvs s a a -> Edh EdhValue
        exitWithResult !evsResult = do
          dto <- getEvsDtype selfEvsObj
          EdhObject
            <$> createArbiHostObjectM'
              clsEvs
              (SomeEventSource evsResult)
              [dto]

        withEvs =
          adaptEdhArg @AnyEventSource other
            >>= \(AnyEventSource (evs :: s' t) _evso) -> case eqT of
              Just (Refl :: t :~: a) -> exitWithResult $
                MappedEvs selfEvs $ \selfEvd ->
                  lingering evs >>= \case
                    Nothing -> return selfEvd -- TODO this okay??
                    Just !rhv -> return $ op selfEvd rhv
              Nothing ->
                throwEdhM UsageError $
                  T.pack $
                    "incompatible event data type: " <> show (typeRep @t)
                      <> " vs "
                      <> show (typeRep @a)

        withValue =
          fromEdh' @a other >>= \case
            Nothing -> return edhNA
            Just !rhv -> exitWithResult $
              MappedEvs selfEvs $ \selfEvd -> return $ op selfEvd rhv

    withEvs <|> withValue

evsDtypeEqProc :: EdhValue -> Edh EdhValue
evsDtypeEqProc !other = do
  !this <- edh'scope'this . contextScope . edh'context <$> edhThreadState
  case edhUltimate other of
    EdhObject !objOther -> (<|> rtnNeg) $
      withDataType objOther $ \ !dtOther ->
        withDataType this $ \ !dtSelf ->
          return $ EdhBool $ isJust $ dtOther `eqDataType` dtSelf
    _ -> rtnNeg
  where
    rtnNeg = return (EdhBool False)

getSinkClass :: Edh Object
getSinkClass =
  importModuleM "eas/RT" >>= \ !moduRT ->
    getObjPropertyM moduRT (AttrByName "Sink") >>= \case
      EdhObject !clsSink -> return clsSink
      _ -> naM "bug: eas/RT provides no Sink class"

getEventSourceClass :: Edh Object
getEventSourceClass =
  importModuleM "eas/RT" >>= \ !moduRT ->
    getObjPropertyM moduRT (AttrByName "EventSource") >>= \case
      EdhObject !clsEventSource -> return clsEventSource
      _ -> naM "bug: eas/RT provides no EventSource class"

getPredefinedEvsDtype :: AttrName -> Edh Object
getPredefinedEvsDtype !dti =
  importModuleM "eas/RT" >>= \ !moduRT ->
    getObjPropertyM moduRT (AttrByName dti) >>= \case
      EdhObject !dto -> return dto
      _ -> naM $ "eas/RT provides no `" <> dti <> "` event sink dtype"

getPredefinedEvsDtype' ::
  forall a. (Typeable a) => AttrName -> Edh (DataType a, Object)
getPredefinedEvsDtype' !dti =
  importModuleM "eas/RT" >>= \ !moduRT ->
    getObjPropertyM moduRT (AttrByName dti) >>= \case
      EdhObject !dto -> withDataType dto $ \(gdt :: DataType a') ->
        case eqT of
          Nothing ->
            naM $
              "requested dtype " <> dti <> " not compatible with host type: "
                <> T.pack (show $ typeRep @a)
          Just (Refl :: a' :~: a) -> return (gdt, dto)
      _ -> naM $ "eas/RT provides no `" <> dti <> "` event sink dtype"
