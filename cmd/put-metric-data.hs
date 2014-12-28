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


dimensions :: O.ReadM [Dimension]
dimensions = text >>= return . fmap (uncurry Dimension) . pairs
    where
        pairs :: Text -> [(Text, Text)]
        pairs = concat . fmap (group . split (== '=')) . split (== ',')
        group :: [a] -> [(a, a)]
        group (x:y:xs) = (x, y) : group xs
        group [] = []
        group _ = fail "could not match pairs"


units :: IO ()
units = mapM_ print $ enumFrom Seconds


putMetricData :: O.Parser PutMetricData
putMetricData = PutMetricData
    <$> O.argument text (O.metavar "<region>")
    <*> O.argument text (O.metavar "<namespace>")
    <*> O.argument text (O.metavar "<metric name>")
    <*> O.argument O.auto (O.metavar "<double value>")
    <*> O.argument O.auto (O.metavar "<unit>")
    -- TODO: OK to use O.many here?
    <*> O.argument dimensions (O.metavar "d1=v1,d2=v2")
    -- TODO: clarify whether this is useful
    <*> O.switch ( O.short 'm'
                <> O.long "metadata"
                <> O.help "Use instance metadata to get authentication info"
                 )


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
