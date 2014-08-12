module Aws.Canonical where

import Data.IORef
import Data.Time.Clock
import Aws.Core

canonicalSigData :: IO SignatureData
canonicalSigData = do
    emptyRef <- newIORef []
    return SignatureData { signatureTimeInfo = AbsoluteTimestamp baseTime
                         , signatureTime = baseTime
                         , signatureCredentials = Credentials "" "" emptyRef Nothing
                         }

baseTime = UTCTime (toEnum 0) $ secondsToDiffTime 0
