{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.RedHat.Internal.Network
( BugzillaServer
, BugzillaContext (..)
, BugzillaApikey (..)
, BugzillaToken (..)
, BugzillaSession (..)
, BugzillaException (..)
, QueryPart
, Request
, requestUrl
, newBzRequest
, sendBzRequest
) where

import Blaze.ByteString.Builder (toByteString)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (Exception, throw)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Typeable
import Network.HTTP.Conduit (Manager, Request(..), Response(..), defaultRequest, host, httpLbs, path, queryString, secure, parseRequest)
import Network.HTTP.Types.URI (QueryText, encodePathSegments, renderQueryText)

type BugzillaServer  = T.Text

-- | Holds information about a 'BugzillaServer' and manages outgoing
-- connections. You can use 'newBugzillaContext' to create one.
data BugzillaContext = BugzillaContext
  { bzServer  :: BugzillaServer
  , bzManager :: Manager
  }

newtype BugzillaToken = BugzillaToken T.Text

newtype BugzillaApikey = BugzillaApikey T.Text

instance FromJSON BugzillaToken where
  parseJSON (Object v) = BugzillaToken <$> v .: "token"
  parseJSON _          = mzero

-- | A session for Bugzilla queries. Use 'anonymousSession' and
-- 'loginSession', as appropriate, to create one.
data BugzillaSession = AnonymousSession BugzillaContext
                     | LoginSession BugzillaContext BugzillaToken
                     | ApikeySession BugzillaContext BugzillaApikey

bzContext :: BugzillaSession -> BugzillaContext
bzContext (AnonymousSession ctx) = ctx
bzContext (LoginSession ctx _)   = ctx
bzContext (ApikeySession ctx _)   = ctx

data BugzillaException
  = BugzillaJSONParseError String
  | BugzillaAPIError Int String
  | BugzillaUnexpectedValue String
    deriving (Show, Typeable)
instance Exception BugzillaException

type QueryPart = (T.Text, Maybe T.Text)

requestUrl :: Request -> B.ByteString
requestUrl req = "https://" <> host req <> path req <> queryString req

sslRequest :: Request
sslRequest =
  defaultRequest {
    secure = True,
    port   = 443
  }

newBzRequest :: BugzillaSession -> [T.Text] -> QueryText -> Request
newBzRequest session methodParts query =
    baseRequest {
      path        = toByteString $ encodePathSegments $ "rest" : methodParts,
      queryString = toByteString $ renderQueryText True queryWithToken
    }
  where
    -- Try to parse the bzServer first, if it has a scheme then use it as the base request,
    -- otherwise force a secure ssl request.
    baseRequest :: Request
    baseRequest = fromMaybe (sslRequest { host = serverBytes }) (parseRequest serverStr)
    serverBytes = TE.encodeUtf8 serverTxt
    serverStr = T.unpack serverTxt
    serverTxt = bzServer . bzContext $ session
    queryWithToken = case session of
                       AnonymousSession _                     -> query
                       LoginSession _ (BugzillaToken token)   -> ("token", Just token) : query
                       ApikeySession _ (BugzillaApikey token) -> ("api_key", Just token) : query

data BzError = BzError Int String
               deriving (Eq, Show)

instance FromJSON BzError where
  parseJSON (Object v) = BzError <$> v .: "code"
                                 <*> v .: "message"
  parseJSON _          = mzero

handleError :: String -> BL.ByteString -> IO b
handleError parseError body = do
  let mError = eitherDecode body
  case mError of
    Left _                   -> throw $ BugzillaJSONParseError parseError
    Right (BzError code msg) -> throw $ BugzillaAPIError code msg

sendBzRequest :: FromJSON a => BugzillaSession -> Request -> IO a
sendBzRequest session req = runResourceT $ do
  response <- liftIO $ httpLbs req . bzManager . bzContext $ session
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> liftIO $ handleError msg (responseBody response)
    Right decoded -> return decoded
