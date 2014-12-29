{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.CloudWatch.Commands.PutMetricAlarm where

import qualified Data.Text as T

import Data.Text (Text)
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Aws.CloudWatch.Core
import Aws.CloudWatch.Types
import Aws.TH

data PutMetricAlarm = PutMetricAlarm
    { ma_alarmActions :: [Text]
    , ma_comparisonOperator :: ComparisonOperator
    , ma_dimensions :: [Dimension]
    , ma_evaluationPeriods :: Integer
    , ma_metricName :: Text
    , ma_name :: Text
    , ma_namespace :: Text
    , ma_period :: Integer
    , ma_region :: Text
    , ma_statistic :: Statistic
    , ma_threshold :: Double
    , ma_unit :: Maybe Unit
    , ma_createTopic :: Bool
    } deriving (Show)

instance SignQuery PutMetricAlarm where
    type ServiceConfiguration PutMetricAlarm = QueryAPIConfiguration
    signQuery PutMetricAlarm{..} = cwSignQuery $
        [ ("Action", qArg "PutMetricAlarm")
        , ("Version", qArg "2010-08-01")
        , ("Namespace", qArg ma_namespace)
        , ("AlarmName", qArg ma_name)
        , ("Period", qShow ma_period)
        , ("EvaluationPeriods", qShow ma_evaluationPeriods)
        , ("Threshold", qShow ma_threshold)
        , ("Statistic", qShow ma_statistic)
        , ("ComparisonOperator", qShow ma_comparisonOperator)
        , ("MetricName", qArg ma_metricName)
        ] +++ enumerate "AlarmActions.member" ma_alarmActions qArg
          +++ enumerateDimensions ma_dimensions

queryValueTransaction ''PutMetricAlarm "PutMetricAlarmResponse"
