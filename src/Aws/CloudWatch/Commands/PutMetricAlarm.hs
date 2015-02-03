{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.CloudWatch.Commands.PutMetricAlarm where

import Data.Text (Text)
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Aws.CloudWatch.Core
import Aws.CloudWatch.Types

data Statistic = SampleCount | Average | Sum | Minimum | Maximum
    deriving (Show)

data ComparisonOperator = GreaterThanOrEqualToThreshold
                        | GreaterThanThreshold
                        | LessThanThreshold
                        | LessThanOrEqualToThreshold
    deriving (Show)

data PutMetricAlarm = PutMetricAlarm
    { ma_actions :: [Text]
    , ma_comparisonOperator :: ComparisonOperator
    , ma_description :: Maybe Text
    , ma_dimensions :: [Dimension]
    , ma_evaluationPeriods :: Integer
    , ma_metricName :: Text
    , ma_name :: Text
    , ma_namespace :: Text
    , ma_period :: Integer
    , ma_statistic :: Statistic
    , ma_threshold :: Integer
    , ma_unit :: Maybe Unit
    } deriving (Show)

instance SignQuery PutMetricAlarm where
    type ServiceConfiguration PutMetricAlarm = QueryAPIConfiguration
    signQuery putMetricAlarm = cwSignQuery $
        [ ("Action", qArg "PutMetricAlarm")
        , ("Version", qArg "2010-08-01")
        , ("Namespace", qArg $ ma_namespace putMetricAlarm)
        ]

queryValueTransaction ''PutMetricAlarm "PutMetricAlarmResponse"
