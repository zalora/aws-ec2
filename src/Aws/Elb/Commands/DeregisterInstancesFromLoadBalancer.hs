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

data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
                        { rilb_name :: Text
                        , rilb_instanceIds :: [Text]
                        } deriving (Show)

enumerateInstanceIds :: [Text] -> HTTP.Query
enumerateInstanceIds = enumerateLists "Instances.member." . fmap unroll
  where
    unroll i = [("InstanceId", qArg i)]

instance SignQuery DeregisterInstancesFromLoadBalancer where
    type ServiceConfiguration DeregisterInstancesFromLoadBalancer = QueryAPIConfiguration
    signQuery DeregisterInstancesFromLoadBalancer{..} = elbSignQuery $
                                                    [ ("Action", qArg "DeregisterInstancesFromLoadBalancer")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg rilb_name)
                                                    ] +++ enumerateInstanceIds rilb_instanceIds

elbValueTransaction ''DeregisterInstancesFromLoadBalancer "DeregisterInstancesFromLoadBalancerResult"
