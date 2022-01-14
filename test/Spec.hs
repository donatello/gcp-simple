-- |
-- Module: Main
-- SPDX-License-Identifier: Apache-2.0
--
-- Main modules for tests.
module Main (main) where

import Network.GCP.Auth.Credentials
import qualified System.Environment as Env
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Hunit

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Tests" [unitTests]

unitTests :: Tasty.TestTree
unitTests =
  Tasty.testGroup
    "Unit Tests"
    [ Hunit.testCase "mkBearerJWT" mkBearerJWTTest
    ]

serviceAccountTestEnv :: String
serviceAccountTestEnv = "SERVICE_ACCOUNT_TEST_FILE"

getOrFail :: String -> (String -> Hunit.Assertion) -> Hunit.Assertion
getOrFail env f = do
  m <- liftIO $ Env.lookupEnv env
  maybe
    (Hunit.assertFailure $ env ++ " not specified")
    f
    m

mkBearerJWTTest :: Hunit.Assertion
mkBearerJWTTest = getOrFail serviceAccountTestEnv $ \fpath -> do
  FromAccount sa <- liftIO $ fromFilePath fpath
  token <- mkBearerJWT sa (Scopes [OAuthScope "abc"])
  putTextLn token

  -- TODO: verify header: alg is RS256, has kid and typ = "JWT"; verify claims
  -- includes aud, exp, iat, iss, and scope.
  Hunit.assertBool "Some checks not verified" False
