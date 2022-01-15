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
    ObjectBody (..),
    PutObjectOptions,
    defaultPutObjectOptions,
  )
where

import Conduit (ConduitT, ResourceT)
import Network.GCP.Simple

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
  m Stream
getObject bucket object opts = undefined

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
putObject bucket object body opts = undefined
