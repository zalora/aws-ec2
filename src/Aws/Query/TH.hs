{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , TemplateHaskell
           #-}

-- boilerplate minimization for experimental stuff
-- also a hub for module re-exports for query commands

module Aws.Query.TH (
  module Aws.Core
, module Aws.Query
, Text
, UTCTime
, FromJSON
, queryValueTransactionDef
, queryValueTransaction
) where

import Language.Haskell.TH

import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson.Types (FromJSON(..))
import Data.Time.Clock (UTCTime)

import Aws.Core
import Aws.Query

queryValueTransactionDef :: Name -> Name -> String -> Name -> Name -> String -> String -> DecsQ
queryValueTransactionDef ty cons tag signF version item filterKey = do
                arg <- newName "arg"
                let tyNameBase = nameBase ty
                [d|
                  instance SignQuery $(conT ty) where
                      type ServiceConfiguration $(conT ty) = QueryAPIConfiguration
                      signQuery ($(conP cons [varP arg])) = $(varE signF) $ [ (B.pack "Action", qArg $ T.pack tyNameBase)
                                                                            , $(varE version)
                                                                            ] +++ enumerate filterKey $(varE arg) qArg

                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ = queryResponseConsumer $ valueConsumerOpt (XMLValueOptions (T.pack item)) (T.pack tag) id

                  instance Transaction $(conT ty) Value
                  |]

queryValueTransaction :: Name -> String -> DecsQ
queryValueTransaction ty tag = [d|
                  instance ResponseConsumer $(conT ty) Value where
                      type ResponseMetadata Value = QueryMetadata
                      responseConsumer _ = queryResponseConsumer $ valueConsumer (T.pack tag) id

                  instance Transaction $(conT ty) Value
                  |]
