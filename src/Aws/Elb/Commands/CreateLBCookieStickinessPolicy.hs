{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , CPP
           #-}

module Aws.Elb.Commands.CreateLBCookieStickinessPolicy where

import Aws.Elb.TH

data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy
                        { clbcsp_loadBalancerName :: Text
                        , clbcsp_cookieExpirationPeriod :: Maybe Integer
                        , clbcsp_policyName :: Text
                        } deriving (Show)

instance SignQuery CreateLBCookieStickinessPolicy where
    type ServiceConfiguration CreateLBCookieStickinessPolicy = QueryAPIConfiguration
    signQuery CreateLBCookieStickinessPolicy{..} = elbSignQuery $
                                                    [ ("Action", qArg "CreateLBCookieStickinessPolicy")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg clbcsp_loadBalancerName)
                                                    , ("PolicyName", qArg clbcsp_policyName)
                                                    ] +++ optional "CookieExpirationPeriod" clbcsp_cookieExpirationPeriod qShow

ELBVALUETRANSACTION(CreateLBCookieStickinessPolicy,"CreateLBCookieStickinessPolicyResult")
