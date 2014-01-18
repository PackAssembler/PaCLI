module PaCLI.Current (Version(..), Current(..), saveBuild, saveCurrent, loadCurrent, fromBuild, compareToBuild) where

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
toVersions mods = catMaybes $ foldl (\acc x -> (toVersion x):acc) [] mods

toVersion :: BP.Mod -> Maybe Version
toVersion mod = if BP.getModTarget mod /= BP.Server then Just $ Version (BP.getModID mod) (BP.getVFilename mod) (BP.getVVersion mod) else Nothing

-- Compare, for updates
compareToBuild :: Current -> BP.Build -> [(Maybe Version, Maybe BP.Mod)]
compareToBuild cur build = findNewOrUpdated cVs bMods ++ findDeleted bMods cVs
    where bMods = BP.getBuildMods build
          cVs = getVersions cur

findNewOrUpdated :: [Version] -> [BP.Mod] -> [(Maybe Version, Maybe BP.Mod)]
findNewOrUpdated vs = catMaybes . foldl (\acc x -> (newOrUpdatedEntry vs x):acc) []

newOrUpdatedEntry :: [Version] -> BP.Mod -> Maybe (Maybe Version, Maybe BP.Mod)
newOrUpdatedEntry vs m = case found of Nothing -> Just (Nothing, Just m)
                                       (Just jfound) -> if getVersion jfound == BP.getVVersion m then Nothing else Just (found, Just m)
    where found = findVersion (BP.getModID m) vs

findDeleted :: [BP.Mod] -> [Version] -> [(Maybe Version, Maybe BP.Mod)]
findDeleted ms = foldl (\acc x -> if modInList (getMod x) ms then acc else (Just x, Nothing):acc) []

findVersion :: String -> [Version] -> Maybe Version
findVersion target = foldl (\acc x -> if getMod x == target then Just x else acc) Nothing

modInList :: String -> [BP.Mod] -> Bool
modInList target = foldl (\acc x -> if BP.getModID x == target then True else acc) False