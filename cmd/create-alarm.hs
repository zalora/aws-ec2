{-# LANGUAGE FlexibleContexts
           , RecordWildCards
           , NamedFieldPuns
           , TypeFamilies  #-}

module Main where

import qualified Data.HashMap.Strict as HM
import qualified Options.Applicative as O

import Aws (simpleAws, Configuration)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative ((<*>), (<$>))
import Data.Aeson.Types (Object(..))
import Control.Monad ((>=>))

import Aws.Cmd
import Aws.CloudWatch
import Aws.SNS


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
    , ca_createTopic :: Bool
    , ca_notificationEmails :: [Text]
    } deriving (Show)


createTopic cfg queryAPIConfiguration createCmd = do
    result <- simpleAws cfg queryAPIConfiguration createCmd
    case getArn result of
        Just arn -> return arn
        _ -> do
            print result
            fail "Cannot create topic!"
    where

        getArn :: Value -> Maybe Text
        getArn = anObject >=> HM.lookup "CreateTopicResult" >=> anObject
                          >=> HM.lookup "TopicArn" >=> aString

        anObject :: Value -> Maybe Object
        anObject (Object v) = Just v
        anObject _ = Nothing

        aString :: Value -> Maybe Text
        aString (String s) = Just s
        aString _ = Nothing


createAlarm :: CreateAlarm -> IO ()
createAlarm ca@CreateAlarm{..} = do
    cfg <- defaultConfiguration
    actions <- if ca_createTopic
        then do
            arn <- createTopic cfg queryAPIConfiguration createCmd
            mapM_ (subscribe cfg arn) ca_notificationEmails
            return [arn]
        else return []
    simpleAws cfg queryAPIConfiguration $ pma actions
    return ()
    where
        queryAPIConfiguration = QueryAPIConfiguration $ encodeUtf8 ca_region
        subscribe cfg arn endpoint =
            simpleAws cfg queryAPIConfiguration $ subscribeCmd arn endpoint
        subscribeCmd topicArn endpoint = Subscribe { s_endpoint = endpoint
                                                   , s_protocol = "email"
                                                   , s_topicArn = topicArn
                                                   }
        pma actions = PutMetricAlarm
            { pma_alarmActions = actions
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
        createCmd = CreateTopic { ct_region = ca_region
                                , ct_name = ca_alarmName }


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
    <*> O.switch (O.long "createTopic")
    <*> O.many (O.option text (makeOption "notificationEmail"))


main :: IO ()
main = O.customExecParser defaultPrefs opts >>= createAlarm
    where
        opts = O.info
            (O.helper <*> createAlarmParser)
            (O.header "AWS CloudWatch PutMetricAlarm client")
