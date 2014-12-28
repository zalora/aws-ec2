{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.SNS.Commands.CreateTopic where

import Aws.Core (SignQuery(..))
import Data.Text (Text)

import Aws.Query.TH
import Aws.SNS.Core

data CreateTopic = CreateTopic
    { ct_region :: Text
    , ct_name :: Text
    } deriving (Show)

instance SignQuery CreateTopic where
    type ServiceConfiguration CreateTopic = QueryAPIConfiguration
    signQuery CreateTopic{..} = snsSignQuery
        [ ("Action", qArg "CreateTopic")
        , defVersion
        , ("Name", qArg ct_name)
        ]

queryValueTransaction ''CreateTopic "CreateTopicResponse"
