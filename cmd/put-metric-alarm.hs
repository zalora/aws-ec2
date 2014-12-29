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
import Aws.SNS


put :: PutMetricAlarm -> IO ()
put pma@PutMetricAlarm{..} = do
    cfg <- defaultConfiguration
    simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ ma_region) pma
    if ma_createTopic
    then do
        simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ ma_region) ct
        return ()
    else return ()
    where ct = CreateTopic { ct_region = ma_region
                           , ct_name = ma_name }


putMetricAlarm :: O.Parser PutMetricAlarm
putMetricAlarm = PutMetricAlarm
    <$> O.many (O.option text (makeOption "alarmActions"))
    <*> O.option O.auto (makeOption "comparisonOperator")
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
