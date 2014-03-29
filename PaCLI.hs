{-# LANGUAGE OverloadedStrings #-}
import Codec.Archive.Zip

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM, when)
import Control.Applicative

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Conduit.Binary (sinkFile)
import Data.Aeson (decode)
import Data.List (nub)
import Data.Conduit
import Data.Maybe

import qualified PaCLI.BuildParser as BP
import qualified PaCLI.Current as Current
import qualified PaCLI.Options as Options

import Network (withSocketsDo)
import Network.HTTP.Conduit

import Options.Applicative

import System.FilePath.Posix
import System.IO (FilePath)
import System.Environment
import System.Directory

-- IO Stuff
main = do
    cmd <- execParser $ info (helper <*> Options.mainParser) idm
    case cmd of
        Options.CmdDownloadBuild x y z -> downloadBuildCommand x y z
        Options.CmdDownloadPack x y z -> downloadPackCommand x y z
        Options.CmdUpdatePack x y z -> updatePackCommand x y z
        Options.CmdCreateZipPack x y -> createZipPackCommand x y
        Options.CmdShowPack x -> showPackCommand x

-- Command Runners
downloadBuildCommand :: Bool -> Bool -> String -> IO ()
downloadBuildCommand config is_server buildID = (getBuild $ packBuildURL buildID) >>= (\d -> downloadBuild config is_server d)

downloadPackCommand :: Bool -> Bool -> String -> IO ()
downloadPackCommand config is_server packID = (getBuild $ latestBuildURL packID) >>= (\d -> downloadBuild config is_server d)

updatePackCommand :: Bool -> Bool -> String -> IO ()
updatePackCommand config is_server packID = (getBuild $ latestBuildURL packID) >>= (\d -> updateToBuild config is_server d)

showPackCommand :: String -> IO ()
showPackCommand packID = doesDirectoryExist packID >>= (\x -> if x then showPack packID else putStrLn "No such pack downloaded")

createZipPackCommand :: String -> String -> IO ()
createZipPackCommand format packID = doesDirectoryExist packID >>= (\x -> when x $ createZipPack format packID)

-- Show Pack
showPack :: String -> IO ()
showPack packID = do
    setCurrentDirectory packID
    c <- Current.loadCurrent
    putStrLn $ "Build: " ++ Current.getBuild c
    putStrLn $ "Minecraft Version: " ++ Current.getMCV c
    putStrLn $ "Forge Version: " ++ Current.getForgeVersion c
    putStrLn "Mods:"
    mapM_ (putStrLn . ("- " ++) . Current.getFilename) $ Current.getVersions c

-- Update/Download Build/Pack Functions
modTargetFilter :: Bool -> BP.Mod -> Bool
modTargetFilter is_server mod = targetFilter is_server (BP.getModTarget mod)

targetFilter :: Bool -> BP.Target -> Bool
targetFilter _ BP.Both    = True
targetFilter False target = case target of BP.Client -> True
                                           BP.Server -> False
targetFilter True target  = case target of BP.Client -> False
                                           BP.Server -> True

downloadBuild :: Bool -> Bool -> BP.Build -> IO ()
downloadBuild config is_server d = do
    let packID = BP.getPackID d
    -- Go to pack directory
    doesDirectoryExist packID >>= (\x -> when x $ removeDirectoryRecursive packID)
    createDirectory packID
    setCurrentDirectory packID
    -- Download mods
    manager <- liftIO $ newManager conduitManagerSettings
    createDirectoryIfMissing False "mods"
    let filteredMods = filter (modTargetFilter is_server) $ BP.getBuildMods d
    mapConcurrently (downloadMod manager) filteredMods
    -- Download config
    when config $ downloadConfig manager (BP.getBuildConfig d)
    -- Save information about what we've just done
    Current.saveBuild (d {BP.getBuildMods = filteredMods})
    -- Go back to original directory
    setCurrentDirectory ".."

updateToBuild :: Bool -> Bool -> BP.Build -> IO ()
updateToBuild config is_server d = do
    let packID = BP.getPackID d
    -- Go to pack directory if it exists, otherwise, just don't do anything
    doesDirectoryExist packID >>= (\x -> if x
        then do
            setCurrentDirectory packID
            -- Download mods
            manager <- liftIO $ newManager conduitManagerSettings
            createDirectoryIfMissing False "mods"
            cur <- Current.loadCurrent
            let filteredMods = filter (modTargetFilter is_server) $ BP.getBuildMods d
                comp = Current.compareToBuild cur filteredMods
            updateMods manager comp
            -- Download config
            when config $ downloadConfig manager (BP.getBuildConfig d)
            -- Save information about what we've just done
            Current.saveBuild (d {BP.getBuildMods = filteredMods})
            -- Go back to original directory
            setCurrentDirectory ".."
        else putStrLn "No such pack downloaded")

updateMods :: Manager -> [(Maybe Current.Version, Maybe BP.Mod)] -> IO [()]
updateMods man = mapConcurrently (updateMod man)

updateMod :: Manager -> (Maybe Current.Version, Maybe BP.Mod) -> IO ()
updateMod man (Nothing, Just m) = downloadMod man m
updateMod man (Just v, Nothing) = removeFileColor ("mods/" ++ Current.getFilename v)
updateMod man (Just v, Just m) = removeFileColor ("mods/" ++ Current.getFilename v) >> downloadMod man m

-- Pack Zipper
createZipPack :: String -> String -> IO ()
createZipPack "technic" packID = do
    -- Go to pack directory
    setCurrentDirectory packID
    -- Download forge
    manager <- liftIO $ newManager conduitManagerSettings
    createDirectoryIfMissing False "bin"
    Current.loadCurrent >>= (\cur -> downloadForge manager cur "bin/modpack.jar")
    -- Get directories we'll be adding
    configExists <- doesDirectoryExist "config"
    let dirs = let must = ["mods", "bin"] in if configExists then "config":must else must
    -- Create zipfile
    archive <- addFilesToArchive [OptRecursive, OptVerbose] emptyArchive dirs
    C.writeFile "technic.zip" (fromArchive archive)
    -- Delete bin
    removeFile "bin/modpack.jar"
    removeDirectory "bin"
    -- Go back to original directory
    setCurrentDirectory ".."
createZipPack "ftb" packID = do
    -- Go to pack directory
    setCurrentDirectory packID
    -- Download forge
    manager <- liftIO $ newManager conduitManagerSettings
    let forgeLoc = "forge.jar"
    Current.loadCurrent >>= (\cur -> downloadForge manager cur forgeLoc)
    -- Get directories we'll be adding
    configExists <- doesDirectoryExist "config"
    let dirs = let must = ["mods"] in if configExists then "config":must else must
    -- Create zipfile
    archive <- addFilesToArchive [OptRecursive, OptVerbose, OptLocation "minecraft" True] emptyArchive dirs
    archive <- foldM addToArchiveIfExists archive ["Info.txt", "420x200Splash.png", "42x42Icon.png"]
    archive <- readEntry [OptVerbose, OptLocation "instMods" True] forgeLoc >>= (\x -> return $ addEntryToArchive x archive)
    C.writeFile "ftb.zip" (fromArchive archive)
    -- Delete forge
    removeFile forgeLoc
    -- Go back to the original directory
    setCurrentDirectory ".."

-- Archive Functions
-- Check for existence before adding to archive
addToArchiveIfExists :: Archive -> FilePath -> IO Archive
addToArchiveIfExists archive filename = doesFileExist filename >>= (\x -> if x then addFilesToArchive [OptVerbose] archive [filename] else return archive)

-- Download Functions
--- URL Builders
baseURL :: String
baseURL = "http://mml.stephenmac.com/"

downloadURL :: String -> String
downloadURL mvid = baseURL ++ "mods/versions/" ++ mvid ++ "/download"

packBuildURL :: String -> String
packBuildURL pbid = baseURL ++ "packs/builds/" ++ pbid

latestBuildURL :: String -> String
latestBuildURL pid = baseURL ++ "packs/" ++ pid ++ "/download"

forgeURL :: String -> String -> String
forgeURL mcv fv = "http://files.minecraftforge.net/maven/net/minecraftforge/forge/" ++ hyphenated ++ "/forge-" ++ hyphenated ++ "-universal.jar"
    where hyphenated = mcv ++ "-" ++ fv

--- Specific Structures
downloadConfig :: Manager -> Maybe String -> IO ()
downloadConfig man Nothing = return ()
downloadConfig man (Just config) = downloadAndExtract man config

downloadMod :: Manager -> BP.Mod -> IO ()
downloadMod manager mod = downloadFile manager (downloadURL $ BP.getVVersion mod) ("mods/" ++ BP.getVFilename mod)

downloadForge :: Manager -> Current.Current -> FilePath -> IO ()
downloadForge manager cur = downloadFile manager $ forgeURL (Current.getMCV cur) (Current.getForgeVersion cur)

getBuild :: String -> IO BP.Build
getBuild url = withSocketsDo $ do
    b <- simpleHttp url
    let (Just d) = decode b
    return d

--- Actions
downloadAndExtract :: Manager -> String -> IO ()
downloadAndExtract man url =
    withSocketsDo . runResourceT $ do
        req <- liftIO $ parseUrl url
        res <- responseBody <$> httpLbs req man
        liftIO $ extractFilesFromArchive [] (toArchive res)

downloadFile :: Manager -> String -> FilePath -> IO ()
downloadFile manager url filename =
    withSocketsDo . runResourceT $ do
        -- Parse the URL to a request.
        req <- liftIO $ parseUrl url

        -- Now get the response
        res <- http (req { responseTimeout = Nothing }) manager

        -- And finally stream the value to a file
        responseBody res $$+- sinkFile filename

        -- Success!
        liftIO . putStrLn $ "\ESC[0;32mDownloaded " ++ filename ++ "\ESC[0m"

removeFileColor :: String -> IO ()
removeFileColor filename = do
    removeFile filename
    putStrLn $ "\ESC[0;31mRemoved " ++ filename ++ "\ESC[0m"
