{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.CreateLBCookieStickinessPolicy where

import Aws.Elb.TH

data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy
                        { clbcsp_loadBalancerName :: Text
                        , clbcsp_cookieExpirationPeriod :: Integer
                        , clbcsp_policyName :: Text
                        } deriving (Show)

instance SignQuery CreateLBCookieStickinessPolicy where
    type ServiceConfiguration CreateLBCookieStickinessPolicy = QueryAPIConfiguration
    signQuery CreateLBCookieStickinessPolicy{..} = elbSignQuery $
                                                    [ ("Action", qArg "CreateLBCookieStickinessPolicy")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg clbcsp_loadBalancerName)
                                                    , ("CookieExpirationPeriod", qShow clbcsp_cookieExpirationPeriod)
                                                    , ("PolicyName", qArg clbcsp_policyName)
                                                    ]

elbValueTransaction ''CreateLBCookieStickinessPolicy "CreateLBCookieStickinessPolicyResult"
