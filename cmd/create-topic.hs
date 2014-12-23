{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}

module Main where

import Control.Monad
import Options.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M

import qualified Aws
import Aws (Configuration(..), LogLevel(..), defaultLog)
import Aws.SNS
import Aws.Query
import Aws.Core (AsMemoryResponse(..))

configuration :: Bool -> IO Configuration
configuration useMetadata = do
    cr <- load
    case cr of
      Nothing -> error "could not locate aws credentials"
      Just cr' -> return Configuration { timeInfo = Aws.Timestamp
                                       , credentials = cr'
                                       , logger = defaultLog Warning
                                       }
  where
    load = if useMetadata then Aws.loadCredentialsFromInstanceMetadata
                          else Aws.loadCredentialsDefault

put :: String -> String -> Bool -> IO ()
put region name useMetadata = do
    cfg <- configuration useMetadata
    response <- Aws.simpleAws cfg (QueryAPIConfiguration $ B.pack region)
        CreateTopic { ct_name = T.pack name }
    print response
    return ()

main = join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = info parser $ header "AWS SNS CreateTopic client"

    parser = put <$> argument str (metavar "<region>")
                 <*> argument str (metavar "<name>")
                 <*> switch (short 'm' <>
                             long "metadata" <>
                             help "Use instance metadata to get authentication info")
