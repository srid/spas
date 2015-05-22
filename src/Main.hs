{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paths_spas (version)

import PostgREST.App
import PostgREST.Middleware
import PostgREST.Error(errResponse)

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Data.ByteString (ByteString)
import System.Environment (getEnv)
import Network.Wai (strictRequestBody)
import Network.Wai.Middleware.Cors (cors)
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai.Middleware.Static (staticPolicy, only)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Data.List (intercalate)
import Data.Version (versionBranch)
import qualified Hasql as H
import qualified Hasql.Postgres as P
import Options.Applicative hiding (columns)

import PostgREST.Config (AppConfig(..), argParser, corsPolicy)

checkCredsEnv :: String -> String -> ByteString -> ByteString -> Bool
checkCredsEnv username password user pass =
  username == (cs user) && password == (cs pass)

main :: IO ()
main = do
  username   <- getEnv "SPAS_USERNAME"
  password   <- getEnv "SPAS_PASSWORD"
  dbUri      <- getEnv "DATABASE_URL"
  portString <- getEnv "PORT"
  let port = read portString :: Int

  -- Build AppConfig type expected by the internals of PostgREST library
  let conf = AppConfig { configDbName=""
             , configDbPort=0
             , configDbUser=""
             , configDbPass=""
             , configDbHost=""
             , configPort=0
             , configAnonRole="TODO"
             , configSecure=False -- TODO
             , configPool=10
             , configV1Schema="public"
             , configJwtSecret=""
             }

  unless (configSecure conf) $
    putStrLn "WARNING, running in insecure mode, auth will be in plaintext"
  unless ("secret" /= configJwtSecret conf) $
    putStrLn "WARNING, running in insecure mode, JWT secret is the default value"
  Prelude.putStrLn $ "Listening on port " ++ (show port)

  let pgSettings = P.StringSettings (cs $ dbUri)
      appSettings = setPort port
                  . setServerName (cs $ "postgrest/" <> prettyVersion)
                  $ defaultSettings
      middle = logStdout
        . (if configSecure conf then redirectInsecure else id)
        . basicAuth (\u p -> return $ (checkCredsEnv username password) u p) "Postgrest realm"
        . gzip def . cors corsPolicy
        . staticPolicy (only [("favicon.ico", "static/favicon.ico")])

  poolSettings <- maybe (fail "Improper session settings") return $
                H.poolSettings (fromIntegral $ configPool conf) 30
  pool :: H.Pool P.Postgres
          <- H.acquirePool pgSettings poolSettings

  runSettings appSettings $ middle $ \req respond -> do
    body <- strictRequestBody req
    resOrError <- liftIO $ H.session pool $ H.tx Nothing $
      (app conf body) req
    either (respond . errResponse) respond resOrError

  where
    prettyVersion = intercalate "." $ map show $ versionBranch version
