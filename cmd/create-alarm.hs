{-# LANGUAGE FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns
           , TypeFamilies  #-}

module Main where

import qualified Options.Applicative as O

import Aws (simpleAws)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative ((<*>), (<$>))

import Aws.Cmd
import Aws.CloudWatch


data CreateAlarm = CreateAlarm
    { ca_comparisonOperator :: ComparisonOperator
    , ca_dimensions :: [Dimension]
    , ca_evaluationPeriods :: Integer
    , ca_metricName :: Text
    , ca_alarmName :: Text
    , ca_namespace :: Text
    , ca_period :: Integer
    , ca_region :: Text
    , ca_statistic :: Statistic
    , ca_threshold :: Double
    , ca_unit :: Maybe Unit
    , ca_topicArn :: Text
    , ca_notificationEmails :: [Text]
    } deriving (Show)


createAlarm :: CreateAlarm -> IO ()
createAlarm ca@CreateAlarm{..} = do
    cfg <- defaultConfiguration
    simpleAws cfg queryAPIConfiguration pma
    return ()
    where
        queryAPIConfiguration = QueryAPIConfiguration $ encodeUtf8 ca_region
        pma = PutMetricAlarm
            { pma_alarmActions = [ca_topicArn]
            , pma_comparisonOperator = ca_comparisonOperator
            , pma_dimensions = ca_dimensions
            , pma_evaluationPeriods = ca_evaluationPeriods
            , pma_metricName = ca_metricName
            , pma_name = ca_alarmName
            , pma_namespace = ca_namespace
            , pma_period = ca_period
            , pma_statistic = ca_statistic
            , pma_threshold = ca_threshold
            }


createAlarmParser :: O.Parser CreateAlarm
createAlarmParser = CreateAlarm
    <$> O.option O.auto (makeOption "comparisonOperator")
    <*> O.many (O.option O.auto (makeOption "dimension"))
    <*> O.option O.auto (makeOption "evaluationPeriods")
    <*> O.option text (makeOption "metricName")
    <*> O.option text (makeOption "alarmName")
    <*> O.option text (makeOption "namespace")
    <*> O.option O.auto (makeOption "period")
    <*> O.option text (makeOption "region")
    <*> O.option O.auto (makeOption "statistic")
    <*> O.option O.auto (makeOption "threshold")
    <*> O.optional (O.option O.auto (makeOption "unit"))
    <*> O.option text (makeOption "topicArn")
    <*> O.many (O.option text (makeOption "notificationEmail"))


main :: IO ()
main = O.customExecParser defaultPrefs opts >>= createAlarm
    where
        opts = O.info
            (O.helper <*> createAlarmParser)
            (O.header "AWS CloudWatch PutMetricAlarm client")
