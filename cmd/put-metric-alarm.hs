{-# LANGUAGE FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns
           , TypeFamilies  #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Options.Applicative as O

import Aws (simpleAws)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative ((<*>), (<$>))

import Aws.Cmd
import Aws.CloudWatch
import Aws.SNS


put :: PutMetricAlarmOption -> IO ()
put pmao@PutMetricAlarmOption{..} = do
    cfg <- defaultConfiguration
    actions <- if pmao_createTopic
        then do
            Object r <- simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ pmao_region) ct
            case HM.lookup "CreateTopicResult" r of
                Just (Object r2) ->
                    case HM.lookup "TopicArn" r2 of
                        Just (String r3) -> return [r3]
        else return []
    simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ pmao_region) $ pma actions
    return ()
    where
        pma actions = PutMetricAlarm
            { pma_alarmActions = actions
            , pma_comparisonOperator = pmao_comparisonOperator
            , pma_dimensions = pmao_dimensions
            , pma_evaluationPeriods = pmao_evaluationPeriods
            , pma_metricName = pmao_metricName
            , pma_name = pmao_alarmName
            , pma_namespace = pmao_namespace
            , pma_period = pmao_period
            , pma_statistic = pmao_statistic
            , pma_threshold = pmao_threshold
            }
        ct = CreateTopic { ct_region = pmao_region
                         , ct_name = pmao_alarmName }


putMetricAlarm :: O.Parser PutMetricAlarmOption
putMetricAlarm = PutMetricAlarmOption
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
    <*> O.switch (O.long "createTopic")


main :: IO ()
main = O.customExecParser defaultPrefs opts >>= put
    where
        opts = O.info
            (O.helper <*> putMetricAlarm)
            (O.header "AWS CloudWatch PutMetricAlarm client")
