{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskell
           , RecordWildCards
           #-}

module Aws.Elb.Commands.ConfigureHealthCheck where

import qualified Data.Text as T
import Aws.Elb.TH

data Target = TargetTCP Integer
            | TargetSSL Integer
            | TargetHTTP Integer Text
            | TargetHTTPS Integer Text

instance Show Target where
  show (TargetTCP port) = "TCP:" ++ show port
  show (TargetSSL port) = "SSL:" ++ show port
  show (TargetHTTP port path) = "HTTP:" ++ show port ++ (T.unpack path)
  show (TargetHTTPS port path) = "HTTPS:" ++ show port ++ (T.unpack path)

data HealthCheck = HealthCheck { hc_target :: Target
                               , hc_healthyThreshold :: Integer
                               , hc_unhealthyThreshold :: Integer
                               , hc_interval :: Integer
                               , hc_timeout :: Integer -- ^ must be less than `hc_interval'
                               } deriving (Show)

data ConfigureHealthCheck = ConfigureHealthCheck { chc_name :: Text
                                                 , chc_healthCheck :: HealthCheck
                                                 }

instance SignQuery ConfigureHealthCheck where
    type ServiceConfiguration ConfigureHealthCheck = QueryAPIConfiguration
    signQuery ConfigureHealthCheck{..} = elbSignQuery $ ($ chc_healthCheck) $ \HealthCheck{..} ->
                                                    [ ("Action", qArg "ConfigureHealthCheck")
                                                    , defVersion
                                                    , ("LoadBalancerName", qArg chc_name)
                                                    , ("HealthCheck.HealthyThreshold", qShow hc_healthyThreshold)
                                                    , ("HealthCheck.UnhealthyThreshold", qShow hc_unhealthyThreshold)
                                                    , ("HealthCheck.Target", qShow hc_target)
                                                    , ("HealthCheck.Interval", qShow hc_interval)
                                                    , ("HealthCheck.Timeout", qShow hc_timeout)
                                                    ]

ELBVALUETRANSACTION(ConfigureHealthCheck,"ConfigureHealthCheckResult")
