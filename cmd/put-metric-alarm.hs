{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           , OverloadedStrings
           #-}

module Main where

import Control.Monad
import Options.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import qualified Aws
import Aws (Configuration(..), LogLevel(..), defaultLog)
import Aws.CloudWatch

configuration :: Bool -> IO Configuration
configuration useMetadata = do
    cr <- load
    case cr of
      Nothing -> error "could not locate aws credentials"
      Just cr' -> return Configuration { timeInfo = Timestamp
                                       , credentials = cr'
                                       , logger = defaultLog Warning
                                       }
  where
    load = if useMetadata then loadCredentialsFromInstanceMetadata
                          else loadCredentialsDefault

put :: String -> String -> String -> String -> ComparisonOperator -> Statistic
    -> Double -> Integer -> Integer -> Unit -> String -> Bool -> IO ()
put region namespace name metricName comparisonOperator statistic
    threshold period evaluationPeriods unit iodims useMetadata = do
    cfg <- configuration useMetadata
    Aws.simpleAws cfg (QueryAPIConfiguration $ B.pack region)
        PutMetricAlarm { ma_actions = []
                       , ma_comparisonOperator = comparisonOperator
                       , ma_dimensions = dimensions
                       , ma_evaluationPeriods = evaluationPeriods
                       , ma_metricName = T.pack metricName
                       , ma_name = T.pack name
                       , ma_namespace = T.pack namespace
                       , ma_period = period
                       , ma_statistic = statistic
                       , ma_threshold = threshold
                       , ma_unit = Just unit
                       }
    return ()
    where dimensions = map (uncurry Dimension) $ pairs $ T.pack iodims

pairs :: Text -> [(Text, Text)]
pairs = concat . fmap (group . T.split (== '=')) . T.split (== ',')
  where
    group (x:y:xs) = (x,y) : group xs
    group [] = []
    group _ = fail "could not match pairs"

main = join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = info parser $ header "AWS CloudWatch PutMetricAlarm client"

    -- TODO: Why doesn't auto work for strings?
    parser = put <$> argument str (metavar "<region>")
                 <*> argument str (metavar "<namespace>")
                 <*> argument str (metavar "<alarm name>")
                 <*> argument str (metavar "<metric name>")
                 <*> argument auto (metavar "<comparison operator>")
                 <*> argument auto (metavar "<statistic>")
                 <*> argument auto (metavar "<threshold>")
                 <*> argument auto (metavar "<period>")
                 <*> argument auto (metavar "<evaluation periods>")
                 <*> argument auto (metavar "<unit>")
                 <*> argument str (metavar "dimension1=value1")
                 <*> switch (short 'm' <>
                             long "metadata" <>
                             help "Use instance metadata to get authentication info")
