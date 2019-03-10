{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Client.Discover (
    complete
  ) where

import           Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

import           Boris.Core.Data.Build
import           Boris.Core.Data.Discover
import           Boris.Core.Data.Instance
import           Boris.Core.Data.Project
import qualified Boris.Client.Response as Response
import           Boris.Client.Request (Request (..))
import qualified Boris.Client.Request as Request
import qualified Boris.Client.Serial.Decode as Decode
import qualified Boris.Client.Serial.Encode as Encode
import           Boris.Representation.ApiV1
import           Boris.Prelude

import qualified Data.Text as Text

import qualified Network.HTTP.Types as HTTP


complete :: DiscoverId -> ProjectName -> [DiscoverInstance] -> Request ()
complete i p ds = do
  Request HTTP.POST (Text.intercalate "/" ["discover", renderBuildId i, "complete"])
    (Response.json 200 $ Decode.wrapper (\PostCompleteResponse -> ()))
    (Request.json . Encode.auto $ error "todo")
