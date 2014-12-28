{-# LANGUAGE FlexibleContexts, TypeFamilies  #-}

module Main where

import qualified Options.Applicative as O

import Aws (simpleAws)
import Data.Text.Encoding (encodeUtf8)
import Options.Applicative ((<*>), (<$>))

import Aws.SNS
import Aws.Cmd


put :: CreateTopic -> IO ()
put ct = do
    cfg <- defaultConfiguration
    response <- simpleAws cfg (QueryAPIConfiguration $ encodeUtf8 $ ct_region ct) ct
    print response
    return ()


createTopic :: O.Parser CreateTopic
createTopic = CreateTopic
    <$> O.option text (makeOption "region")
    <*> O.option text (makeOption "name")


main :: IO ()
main = O.customExecParser defaultPrefs opts >>= put
    where
        opts = O.info
            (O.helper <*> createTopic)
            (O.header "AWS SNS CreateTopic client")
