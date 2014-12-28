{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

module Aws.Cmd where

import qualified Aws
import qualified Options.Applicative as O

import Options.Applicative ((<>), (<*>), (<$>))
import Data.Text (Text, pack)


defaultPrefs = O.ParserPrefs { O.prefMultiSuffix = ""
                             , O.prefDisambiguate = True
                             , O.prefShowHelpOnError = True
                             , O.prefBacktrack = True
                             , O.prefColumns = 80
                             }


makeOption name = O.long name <> O.metavar ("<" ++ name ++ ">")


text :: O.ReadM Text
text = O.str >>= return . pack


configuration :: Bool -> IO Aws.Configuration
configuration useMetadata = do
    cr <- load
    case cr of
      Nothing -> error "could not locate aws credentials"
      Just cr' -> return Aws.Configuration { timeInfo = Aws.Timestamp
                                       , credentials = cr'
                                       , logger = Aws.defaultLog Aws.Warning
                                       }
  where
    load = if useMetadata then Aws.loadCredentialsFromInstanceMetadata
                          else Aws.loadCredentialsDefault

defaultConfiguration :: IO Aws.Configuration
defaultConfiguration = configuration False
