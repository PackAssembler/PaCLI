{-# LANGUAGE OverloadedStrings #-}
module PaCLI.BuildParser (Build(..), Mod(..), Target(..)) where

import Control.Applicative
import Control.Monad (liftM)

import Data.Aeson

-- Data Structures for JSON Parsing
data Build = Build { getBuildID :: String
                   , getBuildMCV :: String
                   , getPackID :: String
                   , getBuildConfig :: Maybe String
                   , getBuildForgeVersion :: String
                   , getPackName :: String
                   , getBuildMods :: [Mod]
                   } deriving (Show)

data Mod = Mod { getVVersion :: String
               , getVFilename :: String
               , getModTarget :: Target
               , getModName :: String
               , getModID :: String
               } deriving (Show)

data Target = Client | Server | Both deriving (Show, Eq, Enum)

-- Instances
instance FromJSON Build where
    parseJSON (Object v) =
        Build <$>
        (v .: "build") <*>
        (v .: "mcv") <*>
        (v .: "id")  <*>
        (v .:? "config") <*>
        (v .: "forge_version") <*>
        (v .: "name") <*>
        (v .: "mods")

instance FromJSON Mod where
    parseJSON (Object v) =
        Mod <$>
        (v .: "version")                 <*>
        (v .: "filename")                <*>
        liftM readTarget (v .: "target") <*>
        (v .: "name")                    <*>
        (v .: "id")

-- Functions
readTarget :: String -> Target
readTarget s
    | s == "client" = Client
    | s == "server" = Server
    | otherwise     = Both
