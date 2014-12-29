{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}

module Main where

import qualified Options.Applicative as O

import Aws (simpleAws)
import Control.Monad (join)
import Data.Text (split)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative ((<>), (<*>), (<$>))

import Aws.CloudWatch
import Aws.Cmd


put :: PutMetricData -> IO ()
put pmd = do
    cfg <- configuration $ pmd_useMetadata pmd
    simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ pmd_region pmd) $ pmd
    return ()


units :: IO ()
units = mapM_ print $ enumFrom Seconds


putMetricData :: O.Parser PutMetricData
putMetricData = PutMetricData
    <$> O.many (O.option O.auto (makeOption "dimension"))
    <*> O.option text (makeOption "metricName")
    <*> O.option text (makeOption "namespace")
    <*> O.option text (makeOption "region")
    <*> O.optional (O.option O.auto (makeOption "unit"))
    <*> O.option O.auto (makeOption "value")

    -- TODO: clarify whether this is useful
    <*> O.switch ( O.short 'm'
                <> O.long "metadata"
                <> O.help "Use instance metadata to get authentication info"
                 )

    -- The following options are for alarms.
    -- TODO: They should be missing or given altogether.
    <*> O.many (O.option text (makeOption "alarmActions"))
    <*> O.optional (O.option text (makeOption "alarmName"))
    <*> O.optional (O.option O.auto (makeOption "comparisonOperator"))
    <*> O.optional (O.option O.auto (makeOption "evaluationPeriods"))
    <*> O.optional (O.option O.auto (makeOption "period"))
    <*> O.optional (O.option O.auto (makeOption "statistic"))
    <*> O.optional (O.option O.auto (makeOption "threshold"))


main :: IO ()
main = join $ O.customExecParser defaultPrefs opts
    where
        opts = O.info parser $ O.header "AWS CloudWatch PutMetricData client"
        parser = O.subparser
            ( O.command "value" (O.info
                (put <$> putMetricData)
                (O.progDesc "put a value metric"))
           <> O.command "units" (O.info
               (O.pure units)
               (O.progDesc "list all metric units"))
            )
