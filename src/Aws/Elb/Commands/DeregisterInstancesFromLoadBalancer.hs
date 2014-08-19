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
                        { dilb_name :: Text
                        , dilb_instanceIds :: [Text]
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
                                                    , ("LoadBalancerName", qArg dilb_name)
                                                    ] +++ enumerateInstanceIds dilb_instanceIds

elbValueTransaction ''DeregisterInstancesFromLoadBalancer "DeregisterInstancesFromLoadBalancerResult"
