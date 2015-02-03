{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Options.Applicative

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

import Aws.Query hiding (optional)
import Aws (simpleAws, Configuration(..), LogLevel(..), defaultLog)
import Aws.Ec2
import qualified Data.Yaml as Y
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import Data.Aeson

import Aws.Cmd

runInst :: String -> String -> String -> Maybe String -> Bool -> Bool -> IO ()
runInst region imageid instType subnetId publicIp useMetadata = do
    cfg <- configuration useMetadata
    val <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $
      RunInstances
        (T.pack imageid)
        (1,1)
        (T.pack instType)
        []
        []
        (T.pack <$> subnetId)
        False
        False
        (Just Stop)
        False
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        publicIp
    B.putStr $ Y.encode val
    return ()

listInst :: String -> Bool -> IO ()
listInst region useMetadata = do
    cfg <- configuration useMetadata
    inst <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ DescribeInstances []
    B.putStr $ Y.encode inst
    return ()

listImg :: String -> Bool -> IO ()
listImg region useMetadata = do
    cfg <- configuration useMetadata
    imageSet <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ DescribeImages []
    B.putStr $ Y.encode imageSet
    return ()

listStatus :: String -> String -> Bool -> IO ()
listStatus region instanceid useMetadata = do
    cfg <- configuration useMetadata
    imageSet <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ DescribeInstanceStatus [T.pack instanceid]
    B.putStr $ Y.encode imageSet
    return ()


termInst :: String -> String -> Bool -> IO ()
termInst region instanceid useMetadata = do
    cfg <- configuration useMetadata
    imageSet <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ TerminateInstances [T.pack instanceid]
    B.putStr $ Y.encode imageSet
    return ()

startInst :: String -> String -> Bool -> IO ()
startInst region instanceid useMetadata = do
    cfg <- configuration useMetadata
    imageSet <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ StartInstances [T.pack instanceid]
    B.putStr $ Y.encode imageSet
    return ()

stopInst :: String -> String -> Bool -> IO ()
stopInst region instanceid useMetadata = do
    cfg <- configuration useMetadata
    imageSet <- simpleAws cfg (QueryAPIConfiguration $ B.pack region) $ StopInstances [T.pack instanceid]
    B.putStr $ Y.encode imageSet
    return ()

main = join $ customExecParser prefs opts
  where
    prefs = ParserPrefs { prefMultiSuffix = ""
                        , prefDisambiguate = True
                        , prefShowHelpOnError = True
                        , prefBacktrack = True
                        , prefColumns = 80
                        }

    opts = parser `info` header "AWS Ec2 client"

    parser = subparser (command "runInst" (argsRunInst runInst `info` progDesc "run instance")
                        <> command "lsInst" (argsListInst listInst `info` progDesc "list all instance")
                        <> command "lsImg" (argsListImg listImg `info` progDesc "list all image")
                        <> command "lsStatus" (argsListStatus listStatus `info` progDesc "list status")
                        <> command "term" (argsInst termInst `info` progDesc "terminate instance")
                        <> command "start" (argsInst startInst `info` progDesc "start instance")
                        <> command "stop" (argsInst stopInst `info` progDesc "stop instance")
                        )

    argsRunInst comm = comm <$> argument str (metavar "<region>")
                       <*> argument str (metavar "<imageid>")
                       <*> argument str (metavar "<insttype>")
                       <*> optional (strOption (short 'n' <>
                                                long "subnetid" <>
                                                help "Use subnetId"))
                       <*> switch (short 'p' <>
                                   long "public" <>
                                   help "Use public-ip with subnetId")
                       <*> switch (short 'm' <>
                                   long "metadata" <>
                                   help "Use instance metadata to get authentication info")
    argsListInst comm = comm <$> argument str (metavar "<region>")
                       <*> switch (short 'm' <>
                                   long "metadata" <>
                                   help "Use instance metadata to get authentication info")
    argsListImg comm = comm <$> argument str (metavar "<region>")
                       <*> switch (short 'm' <>
                                   long "metadata" <>
                                   help "Use instance metadata to get authentication info")
    argsListStatus comm = comm <$> argument str (metavar "<region>")
                          <*> argument str (metavar "<instanceId>")
                          <*> switch (short 'm' <>
                                      long "metadata" <>
                                      help "Use instance metadata to get authentication info")
    argsInst comm = comm <$> argument str (metavar "<region>")
                    <*> argument str (metavar "<instanceId>")
                    <*> switch (short 'm' <>
                                long "metadata" <>
                                help "Use instance metadata to get authentication info")
