{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module: Network.GCP.Storage
-- SPDX-License-Identifier: Apache-2.0
--
-- A simple library for Google Storage.
module Network.GCP.Storage
  ( GetObjectOptions,
    defaultGetObjectOptions,
    Stream,
    getObject,
    putObject,
    ObjectBody (..),
    PutObjectOptions,
    defaultPutObjectOptions,
  )
where

import Conduit (ConduitT, ResourceT)
import Network.GCP.Auth.Credentials
  ( AccessToken (..),
    OAuthScope (OAuthScope),
    OAuthToken (oAuthTokenAccess),
    Scopes (Scopes),
    retrieveTokenFromStore,
  )
import Network.GCP.Simple
  ( GHandle (gHandleStore),
    MonadGSimple (..),
  )
import Network.HTTP.Conduit (requestBodySourceChunked)
import Network.HTTP.Req
  ( GET (GET),
    HttpBody (getRequestBody),
    NoReqBody (NoReqBody),
    POST (POST),
    ReqBodyBs (..),
    defaultHttpConfig,
    https,
    ignoreResponse,
    oAuth2Bearer,
    req,
    reqBr,
    runReq,
    (/:),
    (=:),
  )
import Network.HTTP.Req.Conduit (responseBodySource)

data GetObjectOptions = GetObjectOptions {}
  deriving stock (Eq, Show)

defaultGetObjectOptions :: GetObjectOptions
defaultGetObjectOptions = GetObjectOptions {}

type Stream = ConduitT () ByteString (ResourceT IO) ()

-- | Object is url encoded by this function.
getObject ::
  MonadGSimple m =>
  -- | Bucket name
  Text ->
  -- | Object name
  Text ->
  -- | Start with @defaultGetObjectOptions@
  GetObjectOptions ->
  m ObjectBody
getObject bucket object opts = do
  gh <- getGHandleM
  let getObjScopes =
        Scopes $
          OAuthScope
            <$> [ "https://www.googleapis.com/auth/cloud-platform",
                  "https://www.googleapis.com/auth/cloud-platform.read-only",
                  "https://www.googleapis.com/auth/devstorage.full_control",
                  "https://www.googleapis.com/auth/devstorage.read_only",
                  "https://www.googleapis.com/auth/devstorage.read_write"
                ]
      downloadUrl = https "storage.googleapis.com" /: "storage" /: "v1" /: "b" /: bucket /: "o" /: object

  token <- retrieveTokenFromStore (gHandleStore gh) getObjScopes
  runReq defaultHttpConfig $ do
    let params =
          oAuth2Bearer (encodeUtf8 $ coerce @AccessToken @Text (oAuthTokenAccess token))
            <> "alt" =: ("media" :: Text)
    reqBr
      GET
      downloadUrl
      NoReqBody
      params
      $ \rsp -> return $ OBStream $ responseBodySource rsp

data ObjectBody
  = OBBytes ByteString
  | OBStream Stream

data PutObjectOptions = PutObjectOptions {}
  deriving stock (Eq, Show)

defaultPutObjectOptions :: PutObjectOptions
defaultPutObjectOptions = PutObjectOptions {}

putObject ::
  MonadGSimple m =>
  -- | Bucket
  Text ->
  -- | Object
  Text ->
  -- | Object content
  ObjectBody ->
  -- | Start with @defaultPutObjectOptions@
  PutObjectOptions ->
  m ()
putObject bucket object body opts = do
  gh <- getGHandleM
  let putObjScopes =
        Scopes $
          OAuthScope
            <$> [ "https://www.googleapis.com/auth/cloud-platform",
                  "https://www.googleapis.com/auth/devstorage.full_control",
                  "https://www.googleapis.com/auth/devstorage.read_write"
                ]

  token <- retrieveTokenFromStore (gHandleStore gh) putObjScopes
  runReq defaultHttpConfig $ do
    let params =
          oAuth2Bearer (encodeUtf8 $ coerce @AccessToken @Text (oAuthTokenAccess token))
            <> "uploadType" =: ("media" :: Text)
            <> "name" =: object
        uploadUrl = https "storage.googleapis.com" /: "upload" /: "storage" /: "v1" /: "b" /: bucket /: "o"

    case body of
      OBBytes bs ->
        void $
          req
            POST
            uploadUrl
            (ReqBodyBs bs)
            ignoreResponse
            params
      OBStream stream ->
        void $
          req
            POST
            uploadUrl
            (ReqBodyStream stream)
            ignoreResponse
            params

-- For uploading streams of unknown payload size
newtype ReqBodyStream = ReqBodyStream Stream

instance HttpBody ReqBodyStream where
  getRequestBody (ReqBodyStream stream) = requestBodySourceChunked stream
