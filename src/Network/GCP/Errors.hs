{-# LANGUAGE DerivingStrategies #-}

-- |
-- Module: Network.GCP.Errors
-- SPDX-License-Identifier: Apache-2.0
--
-- Error types and methods.
module Network.GCP.Errors (RefreshError (..), AuthError (..)) where

import Crypto.JWT (JWTError)
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import qualified Network.HTTP.Types as HttpClient

data RefreshError = RefreshError
  { refreshErrorError :: !Text,
    refreshErrorDescription :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

instance FromJSON RefreshError where
  parseJSON = withObject "refresh_error" $ \o ->
    RefreshError
      <$> o .: "error"
      <*> o .:? "error_description"

data AuthError
  = MissingFileError FilePath
  | InvalidFileError FilePath Text
  | SigningError JWTError
  | RefreshReqError HttpClient.Status (Either Text RefreshError)
  | RefreshReqErrorUnknown HttpClient.Status Text
  | ParseErr
  | UnhandledError Text
  deriving stock (Show)

instance Exception AuthError
