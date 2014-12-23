{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.SNS.Commands.CreateTopic where

import qualified Data.Text as T

import Data.Text (Text)
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Aws.SNS.Core
import Aws.TH
import Aws.Core (SignQuery(..))
import Aws.Query
import Aws.Query.TH

data CreateTopic = CreateTopic
    { ct_name :: Text
    } deriving (Show)

instance SignQuery CreateTopic where
    type ServiceConfiguration CreateTopic = QueryAPIConfiguration
    signQuery CreateTopic{..} = snsSignQuery
        [ ("Action", qArg "CreateTopic")
        , defVersion
        , ("Name", qArg ct_name)
        ]

queryValueTransaction ''CreateTopic "CreateTopicResponse"
