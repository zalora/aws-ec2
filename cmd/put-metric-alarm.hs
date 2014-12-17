{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}

module Main where

import Control.Monad
import Options.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Aws
import Aws (Configuration(..), LogLevel(..), defaultLog)
import Aws.CloudWatch

configuration :: Bool -> IO Configuration
configuration useMetadata = do
    cr <- load
    case cr of
      Nothing -> error "could not locate aws credentials"
      Just cr' -> return Configuration { timeInfo = Timestamp
                                       , credentials = cr'
                                       , logger = defaultLog Warning
                                       }
  where
    load = if useMetadata then loadCredentialsFromInstanceMetadata
                          else loadCredentialsDefault

put :: String -> String -> String -> Double -> Unit -> String -> Bool -> IO ()
put region namespace name value unit iodims useMetadata = do
    cfg <- configuration useMetadata
    Aws.simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ PutMetricAlarm (T.pack namespace)
    return ()

main = join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = parser `info` header "AWS CloudWatch PutMetricAlarm client"

    parser = subparser
        (command "value" (args put `info` progDesc "put a value metric"))

    args comm = comm <$> argument str (metavar "<region>")
                     <*> argument str (metavar "<namespace>")
                     <*> argument str (metavar "<metric name>")
                     <*> argument auto (metavar "<double value>")
                     <*> argument auto (metavar "<unit>")
                     <*> argument str (metavar "dimension1=value1")
                     <*> switch (short 'm' <>
                                 long "metadata" <>
                                 help "Use instance metadata to get authentication info")
