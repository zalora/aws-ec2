{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.SetLoadBalancerPoliciesOfListener where

import Aws.Elb.TH

data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener
                        { spl_loadBalancerName :: Text
                        , spl_loadBalancerPort :: Integer
                        , spl_policyNames :: [Text]
                        } deriving (Show)

instance SignQuery SetLoadBalancerPoliciesOfListener where
    type ServiceConfiguration SetLoadBalancerPoliciesOfListener = QueryAPIConfiguration
    signQuery SetLoadBalancerPoliciesOfListener{..} = elbSignQuery $
                                                    [ ("Action", qArg "SetLoadBalancerPoliciesOfListener")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg spl_loadBalancerName)
                                                    , ("LoadBalancerPort", qShow spl_loadBalancerPort)
                                                    ] +++ enumerate "PolicyNames.member" spl_policyNames qArg

ELBVALUETRANSACTION(SetLoadBalancerPoliciesOfListener,"SetLoadBalancerPoliciesOfListenerResult")
