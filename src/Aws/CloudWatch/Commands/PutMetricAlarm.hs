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

data PutMetricAlarmOption = PutMetricAlarmOption
    { pmao_comparisonOperator :: ComparisonOperator
    , pmao_dimensions :: [Dimension]
    , pmao_evaluationPeriods :: Integer
    , pmao_metricName :: Text
    , pmao_alarmName :: Text
    , pmao_namespace :: Text
    , pmao_period :: Integer
    , pmao_region :: Text
    , pmao_statistic :: Statistic
    , pmao_threshold :: Double
    , pmao_unit :: Maybe Unit
    , pmao_createTopic :: Bool
    , pmao_notificationEmails :: [Text]
    } deriving (Show)

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
