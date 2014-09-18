{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , TemplateHaskell
           #-}

module Aws.Ec2.TH (
  module Aws.Core
, module Aws.Query
, module Aws.Ec2.Core
, module Aws.Ec2.Types
, Text
, UTCTime
, FromJSON
, ec2ValueTransactionDef
, ec2ValueTransaction
) where

import Language.Haskell.TH

import Data.Text (Text)
import Data.Aeson.Types (FromJSON(..))
import Data.Time.Clock (UTCTime)

import Aws.Core
import Aws.Ec2.Core
import Aws.Ec2.Types
import Aws.Query
import Aws.Query.TH

ec2ValueTransactionDef :: Name -> Name -> String -> String -> DecsQ
ec2ValueTransactionDef ty cons tag filterKey = queryValueTransactionDef ty cons tag 'ec2SignQuery 'defVersion "item" filterKey

ec2ValueTransaction :: Name -> String -> DecsQ
ec2ValueTransaction = queryValueTransaction
