{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.SNS.Commands.Subscribe where

import Aws.Core (SignQuery(..))
import Data.Text (Text)

import Aws.Query.TH
import Aws.SNS.Core

data Subscribe = Subscribe
    { s_endpoint :: Text
    , s_protocol :: Text
    , s_topicArn :: Text
    } deriving (Show)

instance SignQuery Subscribe where
    type ServiceConfiguration Subscribe = QueryAPIConfiguration
    signQuery Subscribe{..} = snsSignQuery
        [ ("Action", qArg "Subscribe")
        , defVersion
        , ("Endpoint", qArg s_endpoint)
        , ("Protocol", qArg s_protocol)
        , ("TopicArn", qArg s_topicArn)
        ]

queryValueTransaction ''Subscribe "SubscribeResponse"
