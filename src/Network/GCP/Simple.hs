-- |
-- Module: Network.GCP.Simple
-- SPDX-License-Identifier: Apache-2.0
--
-- A simple library for Google Compute Platform
module Network.GCP.Simple
  ( Credentials (..),
    initGHandleFromServiceAccountFile,
  )
where

import Network.GCP.Auth.Credentials
import UnliftIO (MonadUnliftIO, throwIO)

data GHandle = GHandle
  { gHandleCredentials :: Credentials,
    gHandleStore :: Store
  }

initGHandleFromServiceAccountFile :: (MonadUnliftIO m) => FilePath -> Scopes -> m GHandle
initGHandleFromServiceAccountFile f scopes = do
  c <- fromFilePath f
  s <- initStore c scopes
  return $
    GHandle
      { gHandleCredentials = c,
        gHandleStore = s
      }
