{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.CloudWatch.Commands.PutMetricAlarm where

import Data.Text (Text)
import Aws.CloudWatch.Core
import Aws.CloudWatch.Types
import Aws.TH


data PutMetricAlarm = PutMetricAlarm
    { pma_alarmActions :: [Text]
    , pma_comparisonOperator :: ComparisonOperator
    , pma_dimensions :: [Dimension]
    , pma_evaluationPeriods :: Integer
    , pma_metricName :: Text
    , pma_name :: Text
    , pma_namespace :: Text
    , pma_period :: Integer
    , pma_statistic :: Statistic
    , pma_threshold :: Double
    } deriving (Show)

instance SignQuery PutMetricAlarm where
    type ServiceConfiguration PutMetricAlarm = QueryAPIConfiguration
    signQuery PutMetricAlarm{..} = cwSignQuery $
        [ ("Action", qArg "PutMetricAlarm")
        , ("Version", qArg "2010-08-01")
        , ("Namespace", qArg pma_namespace)
        , ("AlarmName", qArg pma_name)
        , ("Period", qShow pma_period)
        , ("EvaluationPeriods", qShow pma_evaluationPeriods)
        , ("Threshold", qShow pma_threshold)
        , ("Statistic", qShow pma_statistic)
        , ("ComparisonOperator", qShow pma_comparisonOperator)
        , ("MetricName", qArg pma_metricName)
        ] +++ enumerate "AlarmActions.member" pma_alarmActions qArg
          +++ enumerateDimensions pma_dimensions

queryValueTransaction ''PutMetricAlarm "PutMetricAlarmResponse"
