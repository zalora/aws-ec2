{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , CPP
           #-}

module Aws.Elb.Commands.RegisterInstancesWithLoadBalancer where

import qualified Network.HTTP.Types as HTTP
import Aws.Elb.TH
import Aws.Elb.Types

data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer
                        { rilb_name :: Text
                        , rilb_instanceIds :: [Text]
                        } deriving (Show)

instance SignQuery RegisterInstancesWithLoadBalancer where
    type ServiceConfiguration RegisterInstancesWithLoadBalancer = QueryAPIConfiguration
    signQuery RegisterInstancesWithLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "RegisterInstancesWithLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg rilb_name)
                                                    ] +++ enumerateInstanceIds rilb_instanceIds

ELBVALUETRANSACTION(RegisterInstancesWithLoadBalancer,"RegisterInstancesWithLoadBalancerResult")
