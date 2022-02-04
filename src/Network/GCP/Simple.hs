-- |
-- Module: Network.GCP.Simple
-- SPDX-License-Identifier: Apache-2.0
--
-- A simple library for Google Compute Platform
module Network.GCP.Simple
  ( Credentials (..),
    initGHandleFromServiceAccountFile,
    MonadGSimple (..),
    GHandle (..),
    runGSimpleMonad,
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

-- class HasGHandle b where
--   getGHandle :: b -> GHandle

-- instance HasGHandle GHandle where
--   getGHandle = identity

class MonadUnliftIO m => MonadGSimple m where
  getGHandleM :: m GHandle

newtype GSimpleMonad a = GSimpleMonad
  { unGSimpleMonad :: ReaderT GHandle IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader GHandle,
      MonadUnliftIO
    )

instance MonadGSimple GSimpleMonad where
  getGHandleM = ask

runGSimpleMonad ::
  GHandle ->
  GSimpleMonad a ->
  IO a
runGSimpleMonad h = flip runReaderT h . unGSimpleMonad
