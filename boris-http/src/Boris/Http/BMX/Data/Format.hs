{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Http.BMX.Data.Format (
    Format (..)
  , renderFormat
  ) where

import           Boris.Prelude

import           Data.Data (Data, Typeable)
import           Data.Serialize
import qualified Data.Text as T

import           GHC.Generics

-- | Formatting control
data Format
  = Strip     -- ^ Strip all adjacent whitespace in some direction
  | Verbatim  -- ^ Leave adjacent nodes intact, don't strip
  deriving (Show, Eq, Data, Typeable, Generic)

instance Serialize Format

renderFormat :: Format -> Text
renderFormat = \case
  Strip    -> "~"
  Verbatim -> T.empty
