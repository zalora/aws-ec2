{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           , CPP
           #-}

module Aws.Elb.Commands.CreateAppCookieStickinessPolicy where

import Aws.Elb.TH

data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy
                        { cacsp_loadBalancerName :: Text
                        , cacsp_cookieName :: Text
                        , cacsp_policyName :: Text
                        } deriving (Show)

instance SignQuery CreateAppCookieStickinessPolicy where
    type ServiceConfiguration CreateAppCookieStickinessPolicy = QueryAPIConfiguration
    signQuery CreateAppCookieStickinessPolicy{..} = elbSignQuery $
                                                    [ ("Action", qArg "CreateAppCookieStickinessPolicy")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg cacsp_loadBalancerName)
                                                    , ("PolicyName", qArg cacsp_policyName)
                                                    , ("CookieName", qArg cacsp_cookieName)
                                                    ]

ELBVALUETRANSACTION(CreateAppCookieStickinessPolicy,"CreateAppCookieStickinessPolicyResult")
