-- | AWS Query API.
--   See http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-query-api.html

{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

module Aws.Query (
  module Aws.Query.Types
, QueryAPIConfiguration(..)
, QueryData(..)
, QueryMetadata(..)
, QueryError(..)
, querySignQuery
, v2SignQuery
, qArg
, qShow
, qBool
, fromJSONConsumer
, valueConsumer
, valueConsumerOpt
, queryResponseConsumer
, (+++)
, optional
, optionalA
, enumerate
, enumerateLists
) where

import qualified Control.Exception as C
import Control.Monad.Trans.Resource (throwM)
import Control.Monad (mplus)

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char8 as Blaze8

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Aeson (FromJSON, Result (..), fromJSON)
import Data.List
import Data.Monoid
import Data.Maybe
import Data.IORef
import Data.Typeable (Typeable)

import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

import qualified Text.XML.Cursor as Cu
import Text.XML.Cursor (($//), ($.//))

import Crypto.Hash (hash, Digest, SHA256)
import Data.Byteable (toBytes)

import Aws.Core
import Aws.Query.Types

data QueryAPIConfiguration qt = QueryAPIConfiguration
                         { qaRegion :: B.ByteString
                         } deriving (Show)

instance DefaultServiceConfiguration (QueryAPIConfiguration NormalQuery) where
  defServiceConfig = QueryAPIConfiguration "us-east-1"
  debugServiceConfig = QueryAPIConfiguration "us-east-1"

data QueryData = QueryData
               { qdEndpoint :: B.ByteString
               , qdRegion :: B.ByteString
               , qdService :: B.ByteString -- ^ matters only for v4 signatures
               } deriving (Show)

data QueryError = QueryError
              { queryStatusCode   :: HTTP.Status
              , queryErrorCode    :: Text
              , queryErrorMessage :: Text
              } deriving (Show, Typeable)

instance C.Exception QueryError

data QueryMetadata = QueryMetadata
                 { requestId :: Maybe Text
                 } deriving (Show)

instance Loggable QueryMetadata where
    toLogText (QueryMetadata r) = "Query: requestId=" <> fromMaybe "<none>" r

instance Monoid QueryMetadata where
    mempty = QueryMetadata Nothing
    (QueryMetadata r1) `mappend` (QueryMetadata r2) = QueryMetadata (r1 `mplus` r2)

data ConsumeError = ConsumeError String deriving (Typeable)

instance C.Exception ConsumeError

instance Show ConsumeError where
  show (ConsumeError e) = e

querySignQuery :: HTTP.Query -> QueryData -> SignatureData -> SignedQuery
querySignQuery query QueryData{..} sd
    = SignedQuery {
        sqMethod = Post
      , sqProtocol = HTTPS
      , sqHost = qdEndpoint
      , sqPort = 443
      , sqPath = "/"
      , sqQuery = []
      , sqDate = Just $ signatureTime sd
      , sqAuthorization = Just auth
      , sqContentType = Just contentType
      , sqContentMd5 = Nothing
      , sqAmzHeaders = [("X-Amz-Date", sigTime)]
      , sqOtherHeaders = []
      , sqBody = Just $ HTTP.RequestBodyBS body
      , sqStringToSign = canonicalRequest
      }
    where
        sigTime = fmtTime "%Y%m%dT%H%M%SZ" $ signatureTime sd

        body = HTTP.renderQuery False query
        contentType = "application/x-www-form-urlencoded"

        bodyHash = Base16.encode $ toBytes (hash body :: Digest SHA256)

        enumHeaders = "content-type;host;x-amz-date"
        canonicalRequest = B.concat [ "POST\n"
                                    , "/\n"
                                    , "\n" -- query string
                                    , "content-type:"
                                    , contentType
                                    , "\n"
                                    , "host:"
                                    , qdEndpoint
                                    , "\n"
                                    , "x-amz-date:"
                                    , sigTime
                                    , "\n"
                                    , "\n" -- end headers
                                    , enumHeaders
                                    , "\n"
                                    , bodyHash
                                    ]

        auth = authorizationV4 sd HmacSHA256 qdRegion qdService
                               enumHeaders
                               canonicalRequest

v2SignQuery :: HTTP.Query -> QueryData -> SignatureData -> SignedQuery
v2SignQuery q QueryData{..} SignatureData{..}
    = SignedQuery {
        sqMethod        = Post
      , sqProtocol      = HTTPS
      , sqHost          = qdEndpoint
      , sqPort          = 443
      , sqPath          = "/"
      , sqQuery         = []
      , sqDate          = Just signatureTime
      , sqAuthorization = Nothing
      , sqContentType   = Just contentType
      , sqContentMd5    = Nothing
      , sqAmzHeaders    = []
      , sqOtherHeaders  = []
      , sqBody          = Just $ HTTP.RequestBodyBS body
      , sqStringToSign  = stringToSign
      }
    where
      contentType     = "application/x-www-form-urlencoded"
      accessKey       = accessKeyID signatureCredentials
      iamTok          = maybe [] (\x -> [("SecurityToken", Just x)]) (iamToken signatureCredentials)

      body            = HTTP.renderQuery False signedQuery
      sig             = signature signatureCredentials HmacSHA256 stringToSign
      signedQuery     = ("Signature", Just sig):expandedQuery

      timestampHeader =
          case signatureTimeInfo of
            AbsoluteTimestamp time -> ("Timestamp", Just $ fmtAmzTime time)
            AbsoluteExpires   time -> ("Expires"  , Just $ fmtAmzTime time)

      newline         = Blaze8.fromChar '\n'

      stringToSign    = Blaze.toByteString . mconcat . intersperse newline $
                            map Blaze.copyByteString
                                ["POST", qdEndpoint, "/"]
                            ++  [HTTP.renderQueryBuilder False expandedQuery]

      expandedQuery   = HTTP.toQuery . sort $ ((iamTok ++ q) ++) [
                            ("AWSAccessKeyId"  , Just accessKey)
                          , ("SignatureMethod" , Just $ amzHash HmacSHA256)
                          , ("SignatureVersion", Just "2")
                          , timestampHeader
                          ]


qArg :: Text -> Maybe B.ByteString
qArg = Just . encodeUtf8

qShow :: Show a => a -> Maybe B.ByteString
qShow = Just . B8.pack . show

qBool :: Bool -> Maybe B.ByteString
qBool True = Just "true"
qBool False = Just "false"

fromJSONConsumer :: FromJSON a => Value -> Response QueryMetadata a
fromJSONConsumer value =
  case fromJSON value of
      Error e -> throwM $
        ConsumeError $ "ConsumeError: " ++ e ++
                       "\nError occured while consuming the following Value:\n" ++
                       show value
      Success result -> return result

valueConsumer :: Text -> (Value -> Response QueryMetadata a) -> Cu.Cursor -> Response QueryMetadata a
valueConsumer = valueConsumerOpt (XMLValueOptions "item")

valueConsumerOpt :: XMLValueOptions -> Text -> (Value -> Response QueryMetadata a) -> Cu.Cursor -> Response QueryMetadata a
valueConsumerOpt options tag cons cu = go $ head cu'
  where
    cu' = cu $.// Cu.laxElement tag
    go = cons . toValue options . Cu.node

-- similar: iamResponseConsumer
queryResponseConsumer :: (Cu.Cursor -> Response QueryMetadata a)
                    -> IORef QueryMetadata
                    -> HTTPResponseConsumer a
queryResponseConsumer inner md resp = xmlCursorConsumer parse md resp
  where
    parse cursor = do
      let rid = listToMaybe $ cursor $// elContent "RequestID"
      tellMetadata $ QueryMetadata rid
      case cursor $// Cu.laxElement "Error" of
          []      -> inner cursor
          (err:_) -> fromError err
    fromError cursor = do
      errCode <- force "Missing Error Code"    $ cursor $// elContent "Code"
      errMsg  <- force "Missing Error Message" $ cursor $// elContent "Message"
      throwM $ QueryError (HTTP.responseStatus resp) errCode errMsg

(+++) :: (Monoid a) => a -> a -> a
(+++) = mappend

optional :: ByteString -> Maybe a -> (a -> Maybe ByteString) -> HTTP.Query
optional k (Just x) f = [(k, f x)]
optional k Nothing f = []

optionalA k v = optional k v qArg

enumerate :: String -> [a] -> (a -> Maybe ByteString) -> HTTP.Query
enumerate k xs f = [(B8.pack $ mconcat [k, ".", show n], f x) | (n, x) <- zip ([1..] :: [Int]) xs]

enumerateLists :: ByteString -> [HTTP.Query] -> HTTP.Query
enumerateLists key xs = mconcat [prefix pairs $ mconcat [key, B8.pack $ show n, "."] | (n, pairs) <- zip ([1..] :: [Int]) xs]
  where
    prefix xs p = [(mconcat [p, k], v) | (k, v) <- xs]
