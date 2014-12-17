{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverloadedStrings
           , RecordWildCards
           , TemplateHaskell
           #-}

module Aws.CloudWatch.Commands.PutMetricData where

import Data.Text (Text)
import Data.Monoid
import Data.Time.Clock (UTCTime)
import Aws.CloudWatch.Core
import Aws.CloudWatch.Types

data MetricDatum = MetricDatum { md_dimensions :: [Dimension]
                               , md_metricName :: Text
                               , md_timestamp :: Maybe UTCTime
                               , md_unit :: Maybe Unit
                               , md_value :: MetricDatumValue
                               } deriving (Show, Eq)

data MetricDatumValue = MetricValue Double
                      | MetricStatisticValue StatisticSet
                      deriving (Show, Eq)

data StatisticSet = StatisticSet { ss_maximum :: Double
                                 , ss_minimum :: Double
                                 , ss_sampleCount :: Double
                                 , ss_sum :: Double
                                 } deriving (Show, Eq)

data PutMetricData = PutMetricData Text [MetricDatum]
                   deriving (Show)

enumerateDimensions :: [Dimension] -> Query
enumerateDimensions = enumerateLists "Dimensions.member." . fmap unroll
  where
    unroll Dimension{..} = [ ("Name", qArg di_name)
                           , ("Value", qArg di_value)
                           ]

enumerateMetrics :: [MetricDatum] -> Query
enumerateMetrics = enumerateLists "MetricData.member." . fmap unroll
  where
    unroll MetricDatum{..} = [ ("MetricName", qArg md_metricName)
                             ] +++ optional "Timestamp" md_timestamp qShow
                               +++ optional "Unit" md_unit qShow
                               +++ enumerateDimensions md_dimensions
                               +++ case md_value of
                                     MetricValue v -> [("Value", qShow v)]
                                     MetricStatisticValue StatisticSet{..} -> [ ("StatisticValues.Maximum", qShow ss_maximum)
                                                                              , ("StatisticValues.Minimum", qShow ss_minimum)
                                                                              , ("StatisticValues.SampleCount", qShow ss_sampleCount)
                                                                              , ("StatisticValues.Sum", qShow ss_sum)
                                                                              ]

instance SignQuery PutMetricData where
    type ServiceConfiguration PutMetricData = QueryAPIConfiguration
    signQuery (PutMetricData namespace xs) = cwSignQuery $ [ ("Action", qArg "PutMetricData")
                                                           , ("Version", qArg "2010-08-01")
                                                           , ("Namespace", qArg namespace)
                                                           ] +++ enumerateMetrics xs

QUERYVALUETRANSACTION(PutMetricData,"PutMetricDataResponse")
