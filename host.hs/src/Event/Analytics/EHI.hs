module Event.Analytics.EHI
  ( installEasBatteries,
    module Event.Analytics.Monad,
    module Event.Analytics.Source,
    module Event.Analytics.DataType,
    module Event.Analytics.XCHG,
    module Event.Analytics.EvsDtArts,
  )
where

-- import           Debug.Trace

import Event.Analytics.DataType
import Event.Analytics.EvsArts
import Event.Analytics.EvsDtArts
import Event.Analytics.Monad
import Event.Analytics.Source
import Event.Analytics.XCHG
import Language.Edh.EHI
import Prelude

installEasBatteries :: EdhWorld -> IO ()
installEasBatteries !world = do
  installModuleM_ world "eas/RT" $ do
    defineEvsArts
