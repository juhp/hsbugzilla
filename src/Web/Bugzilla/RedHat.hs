{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This package is designed to provide an easy-to-use, typesafe
--   interface to querying Bugzilla from Haskell.
--
--   A modified version of Web.Bugzilla to support
--   the list fields in Red Hat's modified bugzilla API.
--
--   A very simple program using this package might look like this:
--
-- >   ctx <- newBugzillaContext "bugzilla.example.org"
-- >   let session = anonymousSession ctx
-- >       user = "me@example.org"
-- >       query = AssignedToField .==. user .&&.
-- >               FlagRequesteeField .==. user .&&.
-- >               (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
-- >   bugs <- searchBugs session query
-- >   mapM_ (putStrLn . show . bugSummary) bugs
--
--   There's a somewhat more in-depth demo program included with the
--   source code to this package.
module Web.Bugzilla.RedHat
( -- * Connecting to Bugzilla
  loginSession
, anonymousSession

, BugzillaServer
, BugzillaSession (..)
, BugzillaToken (..)

  -- * Querying Bugzilla
, searchBugs
, searchBugs'
, searchBugsWithLimit
, searchBugsWithLimit'
, getBug
, getAttachment
, getAttachments
, getComments
, getHistory
, searchUsers
, getUser
, getUserById
, newBzRequest
, sendBzRequest
, intAsText

, BugId
, AttachmentId
, CommentId
, UserId
, EventId
, FlagId
, FlagType
, UserEmail
, Field (..)
, User (..)
, Flag (..)
, Bug (..)
, Attachment (..)
, Comment (..)
, History (..)
, HistoryEvent (..)
, Change (..)
, Modification (..)
, fieldName

, BugzillaException (..)
) where

import Control.Exception (throw, try)
import qualified Data.Text as T

import Web.Bugzilla.RedHat.Internal.Network
import Web.Bugzilla.RedHat.Internal.Search
import Web.Bugzilla.RedHat.Internal.Types

-- makeKey :: String -> String -> Query
-- makeKey k val = [(B.pack k, Just (B.pack val))]

-- | Attempts to create a logged-in 'BugzillaSession' using the
--   provided username and password. Returns 'Nothing' if login
--   fails.
loginSession :: BugzillaServer -> UserEmail -> T.Text -> IO (Maybe BugzillaSession)
loginSession ctx user password = do
  let loginQuery = [("login", Just user),
                    ("password", Just password)]
      session = anonymousSession ctx
      req = newBzRequest session ["login"] loginQuery
  eToken <- try $ sendBzRequest req
  return $ case eToken of
             Left (BugzillaAPIError 300 _) -> Nothing
             Left e                        -> throw e
             Right token                   -> Just $ LoginSession ctx token

-- | Creates an anonymous 'BugzillaSession'. Note that some content
--   will be hidden by Bugzilla when you make queries in this state.
anonymousSession :: BugzillaServer -> BugzillaSession
anonymousSession = AnonymousSession

intAsText :: Int -> T.Text
intAsText = T.pack . show

-- | Searches Bugzilla and returns a list of 'Bug's. The 'SearchExpression'
-- can be constructed conveniently using the operators in "Web.Bugzilla.Search".
searchBugs :: BugzillaSession -> SearchExpression -> IO [Bug]
searchBugs session search = do
  let searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] searchQuery
  (BugList bugs) <- sendBzRequest req
  return bugs

-- | Like 'searchBugs', but returns a list of 'BugId's. You can
-- retrieve the 'Bug' for each 'BugId' using 'getBug'. The combination
-- of 'searchBugs'' and 'getBug' is much less efficient than
-- 'searchBugs'. 'searchBugs'' is suitable for cases where you won't need to call
-- 'getBug' most of the time - for example, polling to determine whether the
-- set of bugs returned by a query has changed.
searchBugs' :: BugzillaSession -> SearchExpression -> IO [BugId]
searchBugs' session search = do
  let fieldsQuery = [("include_fields", Just "id")]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (fieldsQuery ++ searchQuery)
  (BugIdList bugs) <- sendBzRequest req
  return bugs

-- | Search Bugzilla and returns a limited number of results. You can
--   call this repeatedly and use 'offset' to retrieve the results of
--   a large query incrementally. Note that most Bugzillas won't
--   return all of the results for a very large query by default, but
--   you can request this by calling 'searchBugsWithLimit' with 0 for
--   the limit.
searchBugsWithLimit :: BugzillaSession
                    -> Int  -- ^ The maximum number of results to return.
                    -> Int  -- ^ The offset from the first result to start from.
                    -> SearchExpression
                    -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  let limitQuery = [("limit", Just $ intAsText limit),
                    ("offset", Just $ intAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ searchQuery)
  (BugList bugs) <- sendBzRequest req
  return bugs

-- | Like 'searchBugsWithLimit', but returns a list of 'BugId's. See
-- 'searchBugs'' for more discussion.
searchBugsWithLimit' :: BugzillaSession
                     -> Int  -- ^ The maximum number of results to return.
                     -> Int  -- ^ The offset from the first result to start from.
                     -> SearchExpression
                     -> IO [BugId]
searchBugsWithLimit' session limit offset search = do
  let fieldsQuery = [("include_fields", Just "id")]
      limitQuery = [("limit", Just $ intAsText limit),
                    ("offset", Just $ intAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (fieldsQuery ++ limitQuery ++ searchQuery)
  (BugIdList bugs) <- sendBzRequest req
  return bugs

-- | Retrieve a bug by bug number.
getBug :: BugzillaSession -> BugId -> IO (Maybe Bug)
getBug session bid = do
  let req = newBzRequest session ["bug", show bid] []
  (BugList bugs) <- sendBzRequest req
  case bugs of
    [bug] -> return $ Just bug
    []    -> return Nothing
    _     -> throw $ BugzillaUnexpectedValue
                     "Request for a single bug returned multiple bugs"

-- | Retrieve a bug by attachment number.
getAttachment :: BugzillaSession -> AttachmentId -> IO (Maybe Attachment)
getAttachment session aid = do
  let req = newBzRequest session ["bug", "attachment", show aid] []
  (AttachmentList as) <- sendBzRequest req
  case as of
    [a] -> return $ Just a
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single attachment returned multiple attachments"

-- | Get all attachments for a bug.
getAttachments :: BugzillaSession -> BugId -> IO [Attachment]
getAttachments session bid = do
  let req = newBzRequest session ["bug", show bid, "attachment"] []
  (AttachmentList as) <- sendBzRequest req
  return as

-- | Get all comments for a bug.
getComments :: BugzillaSession -> BugId -> IO [Comment]
getComments session bid = do
  let req = newBzRequest session ["bug", show bid, "comment"] []
  (CommentList as) <- sendBzRequest req
  return as

-- | Get the history for a bug.
getHistory :: BugzillaSession -> BugId -> IO History
getHistory session bid = do
  let req = newBzRequest session ["bug", show bid, "history"] []
  sendBzRequest req

-- | Search user names and emails using a substring search.
searchUsers :: BugzillaSession -> T.Text -> IO [User]
searchUsers session text = do
  let req = newBzRequest session ["user"] [("match", Just text)]
  (UserList users) <- sendBzRequest req
  return users

-- | Get a user by email.
getUser :: BugzillaSession -> UserEmail -> IO (Maybe User)
getUser session user = do
  let req = newBzRequest session ["user", T.unpack user] []
  (UserList users) <- sendBzRequest req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"

-- | Get a user by user ID.
getUserById :: BugzillaSession -> UserId -> IO (Maybe User)
getUserById session uid = do
  let req = newBzRequest session ["user", show uid] []
  (UserList users) <- sendBzRequest req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"
