{-# OPTIONS_GHC -Wno-unused-matches #-}

-- |
-- Module: Network.GCP.Storage
-- SPDX-License-Identifier: Apache-2.0
--
-- A simple library for Google Storage.
module Network.GCP.Storage
  ( Stream,
    GetObjectReq,
    getObject,
    InsertObjectReq,
    putObject,
    ObjectBody (..),
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
import qualified Network.HTTP.Req as Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

type Stream = ConduitT () ByteString (ResourceT IO) ()

-- | Set of properties to return. Defaults to noAcl.
data Projection
  = -- | @full@
    -- Include all properties.
    PFull
  | -- | @noAcl@
    -- Omit the owner, acl property.
    PNoACL
  deriving stock (Eq, Ord, Enum, Read, Show, Generic)

instance Hashable Projection

instance FromHttpApiData Projection where
  parseQueryParam = \case
    "full" -> Right PFull
    "noAcl" -> Right PNoACL
    x -> Left ("Unable to parse ObjectsGetProjection from: " <> x)

instance ToHttpApiData Projection where
  toQueryParam = \case
    PFull -> "full"
    PNoACL -> "noAcl"

-- | Used to retrieve an object or its metadata.
data GetObjectReq = GetObjectReq
  { gorIfMetagenerationMatch :: !(Maybe Int64),
    gorIfGenerationNotMatch :: !(Maybe Int64),
    gorIfGenerationMatch :: !(Maybe Int64),
    gorBucket :: !Text,
    gorUserProject :: !(Maybe Text),
    gorIfMetagenerationNotMatch :: !(Maybe Int64),
    gorObject :: !Text,
    gorProjection :: !(Maybe Projection),
    gorGeneration :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)

mkParam :: (ToHttpApiData a) => Text -> Maybe a -> Req.Option scheme
mkParam name = maybe mempty (name =:)

-- | Class to retrieve HTTP optional params from request objects.
class ToReqOption a where
  toReqOption :: a -> Req.Option scheme

instance ToReqOption GetObjectReq where
  toReqOption gor =
    mkParam "ifMetagenerationMatch" (gorIfMetagenerationMatch gor)
      <> mkParam "ifGenerationNotMatch" (gorIfGenerationNotMatch gor)
      <> mkParam "ifGenerationMatch" (gorIfGenerationMatch gor)
      <> mkParam "userProject" (gorUserProject gor)
      <> mkParam "ifMetagenerationNotMatch" (gorIfMetagenerationNotMatch gor)
      <> mkParam "projection" (gorProjection gor)
      <> mkParam "generation" (gorGeneration gor)

-- | Object is url encoded by this function.
getObject ::
  MonadGSimple m =>
  -- | Get Object parameters
  GetObjectReq ->
  m ObjectBody
getObject gor = do
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
      -- TODO url encode object name here.
      downloadUrl = https "storage.googleapis.com" /: "storage" /: "v1" /: "b" /: gorBucket gor /: "o" /: gorObject gor

  token <- retrieveTokenFromStore (gHandleStore gh) getObjScopes
  runReq defaultHttpConfig $ do
    let params =
          oAuth2Bearer (encodeUtf8 $ coerce @AccessToken @Text (oAuthTokenAccess token))
            <> toReqOption gor
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

-- | Apply a predefined set of access controls to this object.
data ObjectsInsertPredefinedACL
  = -- | @authenticatedRead@
    -- Object owner gets OWNER access, and allAuthenticatedUsers get READER
    -- access.
    OIPAAuthenticatedRead
  | -- | @bucketOwnerFullControl@
    -- Object owner gets OWNER access, and project team owners get OWNER
    -- access.
    OIPABucketOwnerFullControl
  | -- | @bucketOwnerRead@
    -- Object owner gets OWNER access, and project team owners get READER
    -- access.
    OIPABucketOwnerRead
  | -- | @private@
    -- Object owner gets OWNER access.
    OIPAPrivate
  | -- | @projectPrivate@
    -- Object owner gets OWNER access, and project team members get access
    -- according to their roles.
    OIPAProjectPrivate
  | -- | @publicRead@
    -- Object owner gets OWNER access, and allUsers get READER access.
    OIPAPublicRead
  deriving stock (Eq, Ord, Enum, Read, Show, Generic)

instance Hashable ObjectsInsertPredefinedACL

instance FromHttpApiData ObjectsInsertPredefinedACL where
  parseQueryParam = \case
    "authenticatedRead" -> Right OIPAAuthenticatedRead
    "bucketOwnerFullControl" -> Right OIPABucketOwnerFullControl
    "bucketOwnerRead" -> Right OIPABucketOwnerRead
    "private" -> Right OIPAPrivate
    "projectPrivate" -> Right OIPAProjectPrivate
    "publicRead" -> Right OIPAPublicRead
    x -> Left ("Unable to parse ObjectsInsertPredefinedACL from: " <> x)

instance ToHttpApiData ObjectsInsertPredefinedACL where
  toQueryParam = \case
    OIPAAuthenticatedRead -> "authenticatedRead"
    OIPABucketOwnerFullControl -> "bucketOwnerFullControl"
    OIPABucketOwnerRead -> "bucketOwnerRead"
    OIPAPrivate -> "private"
    OIPAProjectPrivate -> "projectPrivate"
    OIPAPublicRead -> "publicRead"

-- | Stores a new object and metadata.
data InsertObjectReq = InsertObjectReq
  { iorIfMetagenerationMatch :: !(Maybe Int64),
    iorIfGenerationNotMatch :: !(Maybe Int64),
    iorIfGenerationMatch :: !(Maybe Int64),
    iorPredefinedACL :: !(Maybe ObjectsInsertPredefinedACL),
    iorBucket :: !Text,
    -- iorPayload :: !Object,
    iorBody :: !ObjectBody,
    iorUserProject :: !(Maybe Text),
    iorName :: !Text,
    iorIfMetagenerationNotMatch :: !(Maybe Int64),
    iorContentEncoding :: !(Maybe Text),
    iorKmsKeyName :: !(Maybe Text),
    iorProjection :: !(Maybe Projection)
  }
  deriving stock (Generic)

instance ToReqOption InsertObjectReq where
  toReqOption ior =
    mkParam "ifMetagenerationMatch" (iorIfMetagenerationMatch ior)
      <> mkParam "ifGenerationNotMatch" (iorIfGenerationNotMatch ior)
      <> mkParam "ifGenerationMatch" (iorIfGenerationMatch ior)
      <> mkParam "userProject" (iorUserProject ior)
      <> mkParam "ifMetagenerationNotMatch" (iorIfMetagenerationNotMatch ior)
      <> mkParam "projection" (iorProjection ior)
      <> mkParam "kmsKeyName" (iorKmsKeyName ior)
      <> mkParam "contentEncoding" (iorContentEncoding ior)
      <> mkParam "name" (Just $ iorName ior)
      <> mkParam "uploadType" (Just ("media" :: Text))

-- | Currently only supports simple uploads (i.e. data only, with no metadata)
putObject ::
  MonadGSimple m =>
  -- | Object parameters
  InsertObjectReq ->
  m ()
putObject ior = do
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
            <> toReqOption ior
        uploadUrl = https "storage.googleapis.com" /: "upload" /: "storage" /: "v1" /: "b" /: iorBucket ior /: "o"

    case iorBody ior of
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
