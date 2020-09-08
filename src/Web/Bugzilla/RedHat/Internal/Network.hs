{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Bugzilla.RedHat.Internal.Network
( BugzillaServer
, BugzillaToken (..)
, BugzillaSession (..)
, BugzillaException (..)
, QueryPart
, Request
, newBzRequest
, sendBzRequest
, makeItem
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (Exception, throw)
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Typeable
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types.URI (QueryText, queryTextToQuery)

type BugzillaServer  = String

newtype BugzillaToken = BugzillaToken T.Text

instance FromJSON BugzillaToken where
  parseJSON (Object v) = BugzillaToken <$> v .: "token"
  parseJSON _          = mzero

-- | A session for Bugzilla queries. Use 'anonymousSession' and
-- 'loginSession', as appropriate, to create one.
data BugzillaSession = AnonymousSession BugzillaServer
                     | LoginSession BugzillaServer BugzillaToken

bzContext :: BugzillaSession -> BugzillaServer
bzContext (AnonymousSession ctx) = ctx
bzContext (LoginSession ctx _)   = ctx

data BugzillaException
  = BugzillaJSONParseError String
  | BugzillaAPIError Int String
  | BugzillaUnexpectedValue String
    deriving (Show, Typeable)
instance Exception BugzillaException

type QueryPart = (T.Text, Maybe T.Text)

newBzRequest :: BugzillaSession -> [String] -> QueryText -> Request
newBzRequest session methodParts query =
  setRequestQueryString queryWithToken $
  parseRequest_ $ "https://" ++ server ++ "/" ++ pth
  where
    server = bzContext $ session
    pth = "rest/" ++ intercalate "/" methodParts
    queryWithToken = case session of
                       AnonymousSession _                   -> queryTextToQuery query
                       LoginSession _ (BugzillaToken token) -> makeItem "token" (T.unpack token) : queryTextToQuery query

makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

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

sendBzRequest :: FromJSON a => Request -> IO a
sendBzRequest req = runResourceT $ do
  response <- liftIO $ httpLBS req
  let mResult = eitherDecode $ responseBody response
  case mResult of
    Left msg      -> liftIO $ handleError msg (responseBody response)
    Right decoded -> return decoded
