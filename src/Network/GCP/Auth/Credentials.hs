{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module: Network.GCP.Auth.Credentials
-- SPDX-License-Identifier: Apache-2.0
--
-- Credentials types and methods.
module Network.GCP.Auth.Credentials
  ( Credentials (..),
    ServiceAccount (..),
    fromFilePath,
    initStore,
    retrieveTokenFromStore,
    mkBearerJWT,
    OAuthScope (..),
    OAuthToken (..),
    AccessToken (..),
    Store,
    Scopes (..),
  )
where

import qualified Crypto.JOSE.Compact as Jwt
import qualified Crypto.JWT as Jwt
import Crypto.PubKey.RSA (PrivateKey)
import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime (UTCTime), addUTCTime, getCurrentTime)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Lens.Micro ((.~), (?~))
import Network.GCP.Errors
import qualified Network.HTTP.Client as HttpClient
import Network.HTTP.Req (POST (..), ReqBodyUrlEnc (..), Scheme (Https), Url, bsResponse, defaultHttpConfig, https, renderUrl, req, responseBody, runReq, toVanillaResponse, (/:), (=:))
import qualified Network.HTTP.Types as HttpClient
import System.Directory (doesFileExist)
import UnliftIO (MonadUnliftIO, modifyMVar, throwIO)

newtype ClientId = ClientId Text
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

-- Supported credential mechanisms.
data Credentials = FromAccount !ServiceAccount
  deriving stock (Eq, Show)

-- | Service Account credentials which are typically generated/download
-- from the Google Developer console of the following form:
--
-- > {
-- >   \"type\": \"service_account\",
-- >   \"private_key_id\": \"303ad77e5efdf2ce952DFa\",
-- >   \"private_key\": \"-----BEGIN PRIVATE KEY-----\n...\n\",
-- >   \"client_email\": \"email@serviceaccount.com\",
-- >   \"client_id\": \"035-2-310.useraccount.com\"
-- > }
--
-- The private key is used to sign a JSON Web Token (JWT) of the
-- grant_type @urn:ietf:params:oauth:grant-type:jwt-bearer@, which is sent to
-- 'accountsURL' to obtain a valid 'OAuthToken'. This process requires explicitly
-- specifying which 'Scope's the resulting 'OAuthToken' is authorized to access.
--
-- /See:/ <https://developers.google.com/identity/protocols/OAuth2ServiceAccount#delegatingauthority Delegating authority to your service account>.
data ServiceAccount = ServiceAccount
  { serviceAccountId :: !ClientId,
    serviceAccountEmail :: !Text,
    serviceAccountKeyId :: !Text,
    serviceAccountPrivateKey :: !PrivateKey,
    serviceAccountAccountUser :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON ServiceAccount where
  parseJSON = withObject "service_account" $ \o -> do
    bs <- encodeUtf8 @Text @ByteString <$> o .: "private_key"
    k <- case listToMaybe (readKeyFileFromMemory bs) of
      Just (PrivKeyRSA k) -> pure k
      _ ->
        fail "Unable to parse key contents from \"private_key\""
    ServiceAccount
      <$> o .: "client_id"
      <*> o .: "client_email"
      <*> o .: "private_key_id"
      <*> pure k
      <*> pure Nothing

-- | Load Service Account from file.
fromFilePath :: MonadUnliftIO m => FilePath -> m Credentials
fromFilePath f = do
  p <- liftIO (doesFileExist f)
  unless p $
    throwIO (MissingFileError f)
  bs <- liftIO (readFileBS f)
  either
    (throwIO . InvalidFileError f . toText)
    pure
    (fromJSONCredentials bs)

fromJSONCredentials :: ByteString -> Either String Credentials
fromJSONCredentials bs = do
  let sa = FromAccount <$> eitherDecodeStrict bs
  case sa of
    Left e -> Left $ "Failed parsing service account: " ++ e
    Right v -> Right v

data Auth = Auth
  { authCredentials :: !Credentials,
    authToken :: OAuthToken
  }
  deriving stock (Eq, Show)

-- | Data store for thread safe access to credentials.
newtype Store = Store (MVar Auth)

initStore :: (MonadUnliftIO m) => Credentials -> Scopes -> m Store
initStore creds scopes = do
  auth <- fmap (Auth creds) action
  Store <$> newMVar auth
  where
    action = case creds of
      FromAccount sa -> serviceAccountToken sa scopes

retrieveTokenFromStore :: (MonadUnliftIO m) => Store -> Scopes -> m OAuthToken
retrieveTokenFromStore (Store s) scopes = do
  x <- liftIO $ readMVar s
  isValid <- validate x
  if isValid
    then pure $ authToken x
    else liftIO . modifyMVar s $ \y@(Auth c@(FromAccount sa) _) -> do
      isValidY <- validate y
      if isValidY
        then pure (y, authToken y)
        else do
          z <- serviceAccountToken sa scopes
          pure (Auth c z, z)

validate :: MonadIO m => Auth -> m Bool
validate (Auth _ token) = do
  currTime <- liftIO getCurrentTime
  return $ oAuthTokenExpiry token < currTime

-- | List of allowed scopes for authorization.
newtype Scopes = Scopes [OAuthScope]
  deriving stock (Eq, Show)

mkScopesStr :: Scopes -> Text
mkScopesStr (Scopes s) = T.intercalate " " $ map (\(OAuthScope t) -> t) s

serviceAccountToken :: (MonadUnliftIO m) => ServiceAccount -> Scopes -> m OAuthToken
serviceAccountToken sa scopes = do
  b <- mkBearerJWT sa scopes

  -- make the refresh request.
  runReq defaultHttpConfig $ do
    let params =
          "grant_type" =: ("urn:ietf:params:oauth:grant-type:jwt-bearer" :: Text)
            <> "assertion" =: b
    brsp <-
      req
        POST
        (https "www.googleapis.com" /: "oauth2" /: "v4" /: "token")
        (ReqBodyUrlEnc params)
        bsResponse
        mempty
    if httpStatus brsp == HttpClient.status200
      then success brsp
      else failure brsp
  where
    httpStatus resp = HttpClient.responseStatus $ toVanillaResponse resp

    success rsp = do
      t <- liftIO getCurrentTime
      either
        (throwIO . RefreshReqError (httpStatus rsp) . Left)
        (\f -> return (f t))
        (parseBS $ responseBody rsp)
    failure rsp =
      throwIO $
        either (RefreshReqErrorUnknown $ httpStatus rsp) (RefreshReqError (httpStatus rsp) . Right) $
          parseBS $ responseBody rsp

parseBS :: FromJSON a => ByteString -> Either Text a
parseBS = either (Left . toText) Right . eitherDecodeStrict'

-- | Maximum lifetime of JWTs generated for auth.
maxTokenLifetime :: NominalDiffTime
maxTokenLifetime = 3600

-- | @https://www.googleapis.com/oauth2/v4/token@.
tokenURL :: Url 'Https
tokenURL = https "www.googleapis.com" /: "oauth2" /: "v4" /: "token"

mkBearerJWT :: (MonadUnliftIO m) => ServiceAccount -> Scopes -> m Text
mkBearerJWT sa scopes = do
  UTCTime d dt <- liftIO getCurrentTime

  let -- Truncate time to second.
      t = UTCTime d $ fromIntegral (truncate dt :: Int64)

      jwk = Jwt.fromRSA $ serviceAccountPrivateKey sa

      claims_ =
        Jwt.emptyClaimsSet
          & Jwt.claimAud ?~ Jwt.Audience [fromString $ toString $ renderUrl tokenURL]
          & Jwt.claimIat ?~ Jwt.NumericDate t
          & Jwt.claimExp ?~ Jwt.NumericDate (addUTCTime maxTokenLifetime t)
          & Jwt.claimIss ?~ fromString (toString $ serviceAccountEmail sa)
          & Jwt.unregisteredClaims .~ Map.singleton "scope" (String $ mkScopesStr scopes)

      claims =
        -- Add sub claim only if account user is present
        maybe
          claims_
          (\v -> claims_ & Jwt.claimSub ?~ v)
          (fromString . toString <$> serviceAccountAccountUser sa)

  r <- liftIO $
    runExceptT $ do
      let header =
            Jwt.newJWSHeader ((), Jwt.RS256)
              & Jwt.typ ?~ Jwt.HeaderParam () "JWT"
              & Jwt.kid ?~ Jwt.HeaderParam () (serviceAccountKeyId sa)
      signedJwt <-
        Jwt.signClaims jwk header claims
      return $ toStrict $ Jwt.encodeCompact signedJwt
  either (throwIO . SigningError) (return . decodeUtf8) r

{-
The '_tokenAccess' field will be inserted verbatim into the
@Authorization: Bearer ...@ header for all HTTP requests.
-}
data OAuthToken = OAuthToken
  { oAuthTokenAccess :: !AccessToken,
    oAuthTokenRefresh :: !(Maybe RefreshToken),
    oAuthTokenExpiry :: !UTCTime
  }
  deriving stock (Eq, Show)

instance FromJSON (UTCTime -> OAuthToken) where
  parseJSON = withObject "bearer" $ \o -> do
    t <- o .: "access_token"
    r <- o .:? "refresh_token"
    e <- o .: "expires_in" <&> fromInteger
    pure (OAuthToken t r . addUTCTime e)

-- | An OAuth2 scope.
newtype OAuthScope = OAuthScope Text
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
      -- FromHttpApiData,
      -- ToHttpApiData,
    )
  deriving newtype (IsString)
  deriving anyclass (FromJSON, ToJSON)

-- | An OAuth2 access token.
newtype AccessToken = AccessToken Text
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
      -- FromHttpApiData,
      -- ToHttpApiData,
    )
  deriving newtype (IsString)
  deriving anyclass (FromJSON, ToJSON)

-- | An OAuth2 refresh token.
newtype RefreshToken = RefreshToken Text
  deriving stock
    ( Eq,
      Ord,
      Show,
      Read,
      Generic
      -- FromHttpApiData,
      -- ToHttpApiData,
    )
  deriving newtype (IsString)
  deriving anyclass (FromJSON, ToJSON)
