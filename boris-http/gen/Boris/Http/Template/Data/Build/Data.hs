{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Boris.Http.Template.Data.Build.Data where
import qualified Projector.Html.Runtime
data Build
    = Build {buildId :: !Projector.Html.Runtime.Text,
             buildLog :: !HasLog,
             buildProject :: !Projector.Html.Runtime.Text,
             buildBuild :: !Projector.Html.Runtime.Text,
             buildRef :: !(Projector.Html.Runtime.Maybe Projector.Html.Runtime.Text),
             buildCommit :: !(Projector.Html.Runtime.Maybe Projector.Html.Runtime.Text)}
data BuildStatus = BuildOk | BuildKo | BuildUndecided
data HasLog = HasLog | NoLog
