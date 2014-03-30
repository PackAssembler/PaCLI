module PaCLI.Current (Version(..), Current(..), UpdateAction(..), saveBuild, saveCurrent, loadCurrent, fromBuild, compareToBuild) where

import Data.Maybe

import qualified PaCLI.BuildParser as BP

data Version = Version { getMod :: String -- The ModID
                       , getFilename :: String -- The filename of the mod, not including the path
                       , getVersion :: String -- The VersionID, a unique identifier of the version
                       } deriving (Show, Read)

data Current = Current { getBuild :: String -- The BuildID
                       , getMCV :: String -- Minecraft Version
                       , getForgeVersion :: String -- Forge version
                       , getVersions :: [Version] -- The versions that are currently on the machine
                       } deriving (Show, Read)

data UpdateAction = Install BP.Mod | Replace Version BP.Mod | Remove Version deriving (Show)

currentFile :: String
currentFile = "current.dat"

saveBuild :: BP.Build -> IO ()
saveBuild = saveCurrent . fromBuild

saveCurrent :: Current -> IO ()
saveCurrent = writeFile currentFile . show

loadCurrent :: IO Current
loadCurrent = do
    cData <- readFile currentFile
    return $ read cData

fromBuild :: BP.Build -> Current
fromBuild (BP.Build buildID mcv _ _ fv _ mods) = Current buildID mcv fv $ toVersions mods

toVersions :: [BP.Mod] -> [Version]
toVersions = foldl (\acc x -> toVersion x : acc) []

toVersion :: BP.Mod -> Version
toVersion mod = Version (BP.getModID mod) (BP.getVFilename mod) $ BP.getVVersion mod

-- Compare, for updates
compareToBuild :: Current -> [BP.Mod] -> [UpdateAction]
compareToBuild cur bMods = findNewOrUpdated cVs bMods ++ findDeleted bMods cVs
    where cVs = getVersions cur

findNewOrUpdated :: [Version] -> [BP.Mod] -> [UpdateAction]
findNewOrUpdated vs = catMaybes . foldl (\acc x -> newOrUpdatedEntry vs x : acc) []

newOrUpdatedEntry :: [Version] -> BP.Mod -> Maybe UpdateAction
newOrUpdatedEntry vs m = case found of Nothing -> Just $ Install m
                                       (Just jfound) -> if getVersion jfound == BP.getVVersion m then Nothing else Just $ Replace jfound m
    where found = findVersion (BP.getModID m) vs

findDeleted :: [BP.Mod] -> [Version] -> [UpdateAction]
findDeleted ms = foldl (\acc x -> if modInList (getMod x) ms then acc else Remove x : acc) []

findVersion :: String -> [Version] -> Maybe Version
findVersion target = foldl (\acc x -> if getMod x == target then Just x else acc) Nothing

modInList :: String -> [BP.Mod] -> Bool
modInList target = foldl (\acc x -> ((BP.getModID x == target) || acc)) False