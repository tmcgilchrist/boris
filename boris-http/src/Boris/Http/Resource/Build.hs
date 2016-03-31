{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.Resource.Build (
    collection
  , item
  ) where


import           Airship (Resource (..), Webmachine, defaultResource, lookupParam, putResponseBody, appendRequestPath)

import           Boom.Airship (notfound)

import           Boris.Core.Data
import           Boris.Http.Airship
import           Boris.Http.Data
import           Boris.Http.Repository
import           Boris.Http.Representation.Build
import           Boris.Http.Version
import           Boris.Store.Build (BuildData (..))
import qualified Boris.Store.Build as SB
import qualified Boris.Store.Index as SI
import qualified Boris.Store.Tick as ST
import           Boris.Queue (BuildQueue (..), Request (..))
import qualified Boris.Queue as Q

import           Charlotte.Airship (PostHandler (..), withVersionJson)
import           Charlotte.Airship (processPostMedia, jsonResponse, setResponseHeader)

import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)
import           Data.Time (getCurrentTime, diffUTCTime)

import           Mismi (runAWS, runAWST, renderError)
import           Mismi.Amazonka (Env)

import qualified Network.HTTP.Types as HTTP

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (bimapEitherT)

collection :: Env -> Environment -> BuildQueue -> ConfigLocation -> Resource IO
collection env e q c =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet, HTTP.methodPost, HTTP.methodDelete]

    , contentTypesProvided = return . withVersionJson $ \v -> case v of
        V1 -> do
          b <- getBuild
          p <- getProject
          rs <- webT renderError . runAWS env $ SI.getBuildRefs e p b
          r <- fmap (GetBuilds p b) . forM rs $ \r -> do
            is <- webT renderError . runAWS env $ SI.getBuildIds e p b r
            pure $ GetBuildsDetail r is
          pure . jsonResponse $ r

    , processPost = processPostMedia . withVersionJson $ \v -> case v of
        V1 -> do
          b <- getBuild
          p <- getProject
          repository <- webT renderConfigError (pick env c p) >>= notfound
          i <- webT id . runAWST env renderError . bimapEitherT ST.renderTickError id $ ST.next e p b
          webT id . runAWST env renderError . bimapEitherT SB.renderRegisterError id $ SB.register e p b i
          let req = Request i p repository b Nothing -- FIX COMPLETE ref needs to be parsed from body
          webT renderError . runAWS env $ Q.put q req
          putResponseBody . jsonResponse $ GetBuild (BuildData i p b Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
          setLocation ["builds"]
          pure $ PostResponseLocation [renderBuildId i]

    }

item :: Env -> Environment -> Resource IO
item env e =
  defaultResource {
      allowedMethods = pure [HTTP.methodGet]

    , contentTypesProvided = pure . withVersionJson $ \v -> case v of
        V1 -> do
          now <- liftIO getCurrentTime
          i <- getBuildId
          b <- webT id . runAWST env renderError . bimapEitherT SB.renderFetchError id $ SB.fetch e i
          case buildDataHeartbeatTime b of
            Nothing ->
              pure . jsonResponse $ GetBuild b
            Just h ->
              if diffUTCTime now h > 120
                 then do
                    void . webT id . bimapEitherT renderError id . runAWS env $ SB.cancel e i
                    pure . jsonResponse . GetBuild $ b { buildDataResult = Just . fromMaybe BuildKo . buildDataResult $ b }
                 else
                    pure . jsonResponse $ GetBuild b


    , deleteCompleted =
        pure False

    , deleteResource = do
        i <- getBuildId
        webT id . bimapEitherT renderError id . runAWS env $ SB.cancel e i
    }

getBuild :: Webmachine IO Build
getBuild =
  Build <$> lookupParam "build-name"

getProject :: Webmachine IO Project
getProject =
  Project <$> lookupParam "project-name"

getBuildId :: Webmachine IO BuildId
getBuildId =
  BuildId <$> lookupParam "build-id"

setLocation :: [Text] -> Webmachine IO ()
setLocation p =
  appendRequestPath p >>= setResponseHeader . (,) HTTP.hLocation
