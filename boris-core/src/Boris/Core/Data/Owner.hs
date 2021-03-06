{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Boris.Core.Data.Owner (
    Source (..)
  , renderSource
  , sourceFromInt
  , sourceToInt
  , Owner (..)
  , OwnerId (..)
  , OwnerName (..)
  , OwnerType (..)
  , ownerTypeFromInt
  , ownerTypeToInt
  ) where

import           Boris.Core.Data.Repository
import           Boris.Prelude

data Source =
    GithubSource
  | BorisSource
    deriving (Eq, Ord, Show, Enum, Bounded)

renderSource :: Source -> Text
renderSource s =
  case s of
    GithubSource ->
      "github"
    BorisSource ->
      "boris"

sourceToInt :: Source -> Int64
sourceToInt s =
  case s of
    GithubSource ->
      0
    BorisSource ->
      1

sourceFromInt :: Int64 -> Maybe Source
sourceFromInt n =
  case n of
    0 ->
      Just GithubSource
    1 ->
      Just BorisSource
    _ ->
      Nothing

newtype OwnerId =
  OwnerId {
      getOwnerId :: Int64
    } deriving (Eq, Ord, Show)

newtype OwnerName =
  OwnerName {
      getOwnerName :: Text
    } deriving (Eq, Ord, Show)

data OwnerType =
    GithubUserOwnerType
  | GithubOrganisationOwnerType
  | BorisOwnerType
    deriving (Eq, Ord, Show, Enum, Bounded)

ownerTypeToInt :: OwnerType -> Int64
ownerTypeToInt o =
  case o of
    BorisOwnerType ->
      0
    GithubUserOwnerType ->
      1
    GithubOrganisationOwnerType ->
      2

ownerTypeFromInt :: Int64 -> Maybe OwnerType
ownerTypeFromInt n =
  case n of
    0 ->
      Just BorisOwnerType
    1 ->
      Just GithubUserOwnerType
    2 ->
      Just GithubOrganisationOwnerType
    _ ->
      Nothing

data Owner =
  Owner {
      ownerId :: OwnerId
    , ownerName :: OwnerName
    , ownerType :: OwnerType
    } deriving (Eq, Ord, Show)
