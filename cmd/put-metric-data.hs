{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}

module Main where

import qualified Options.Applicative as O

import Control.Monad
import Options.Applicative

import Aws (simpleAws)
import Data.Text (split)
import Data.Text.Encoding (encodeUtf8)

import Aws.CloudWatch
import Aws.Cmd

put :: Text -> Text -> Text -> Double -> Unit -> [Dimension] -> Bool -> IO ()
put region namespace name value unit dimensions useMetadata = do
    cfg <- configuration useMetadata
    m <- metric
    simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 region) $ PutMetricData namespace [m]
    return ()
  where
    metric = return MetricDatum
        { md_dimensions = dimensions
        , md_metricName = name
        , md_timestamp = Nothing
        , md_unit = Just unit
        , md_value = MetricValue value
        }

units :: IO ()
units = mapM_ print $ enumFrom Seconds

main = join $ customExecParser defaultPrefs opts
  where

    opts = parser `info` header "AWS CloudWatch PutMetricData client"

    parser = subparser
        ( command "value" (args `info` progDesc "put a value metric")
       <> command "units" (pure units `info` progDesc "list all metric units")
        )

    args = put <$> argument text (metavar "<region>")
               <*> argument text (metavar "<namespace>")
               <*> argument text (metavar "<metric name>")
               <*> argument auto (metavar "<double value>")
               <*> argument auto (metavar "<unit>")
               <*> O.many (O.argument O.auto (metavar "<dimension>"))
               <*> switch (short 'm' <>
                           long "metadata" <>
                           help "Use instance metadata to get authentication info")
