-- |
-- Copyright: (c) 2022 Aditya Manthramurthy
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Aditya Manthramurthy <aditya.mmy@gmail.com>
--
-- A simple library for Google Compute Platform
module Network.GCP.Simple
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
