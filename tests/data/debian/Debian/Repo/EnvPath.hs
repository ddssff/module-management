{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Debian.Repo.EnvPath
    ( EnvRoot(..)
    , EnvPath(..)
    , outsidePath
    , appendPath
    , rootEnvPath
    ) where

-- |The root directory of an OS image.
data EnvRoot = EnvRoot { rootPath :: FilePath } deriving (Ord, Eq, Read, Show)

-- |A directory inside of an OS image.
data EnvPath = EnvPath { envRoot :: EnvRoot
                       , envPath :: FilePath
                       } deriving (Ord, Eq, Read, Show)

outsidePath :: EnvPath -> FilePath
outsidePath path = rootPath (envRoot path) ++ envPath path

appendPath :: FilePath -> EnvPath -> EnvPath
appendPath suff path = path { envPath = envPath path ++ suff }

rootEnvPath :: FilePath -> EnvPath
rootEnvPath s = EnvPath { envRoot = EnvRoot "", envPath = s }
