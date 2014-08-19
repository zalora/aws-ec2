{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.DeregisterInstancesFromLoadBalancer where

import qualified Network.HTTP.Types as HTTP
import Aws.Elb.TH
import Aws.Elb.Types

data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
                        { dilb_name :: Text
                        , dilb_instanceIds :: [Text]
                        } deriving (Show)

instance SignQuery DeregisterInstancesFromLoadBalancer where
    type ServiceConfiguration DeregisterInstancesFromLoadBalancer = QueryAPIConfiguration
    signQuery DeregisterInstancesFromLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "DeregisterInstancesFromLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg dilb_name)
                                                    ] +++ enumerateInstanceIds dilb_instanceIds

elbValueTransaction ''DeregisterInstancesFromLoadBalancer "DeregisterInstancesFromLoadBalancerResult"
