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
chooseDir :: Options.OptionGroup -> String
chooseDir (Options.PackOptions maybeDir packId) = fromMaybe packId maybeDir

cleanDirectory :: IO ()
cleanDirectory = do
    doesDirectoryExist "mods" >>= flip when (removeDirectoryRecursive "mods")
    doesFileExist "current.dat" >>= flip when (removeFile "current.dat")

directorize :: Bool -> Options.OptionGroup -> IO Bool
directorize clean opts = let dir = chooseDir opts in doesDirectoryExist dir >>= (\x -> if clean then
    (if x then setCurrentDirectory dir >> cleanDirectory else createDirectory dir >> setCurrentDirectory dir) >> return True else
    (if x then setCurrentDirectory dir >> return True else putStrLn "No Such Pack Downloaded!" >> return False))

main = do
    cmd <- execParser $ info (helper <*> Options.mainParser) idm
    case cmd of
        Options.CmdDownloadPack buildn (Options.DownloadOptions config is_server) packOpts -> do
            directorize True packOpts
            getBuild (packBuildURL (Options.getPackId packOpts) buildn) >>= downloadBuild config is_server

        Options.CmdUpdatePack (Options.DownloadOptions config is_server) packOpts -> do
            possible <- directorize False packOpts
            when possible $ getBuild (packBuildURL (Options.getPackId packOpts) "-1") >>= updateToBuild config is_server

        Options.CmdCreateZipPack format packOpts -> do
            possible <- directorize False packOpts
            when possible $ createZipPack format (Options.getPackId packOpts)

        Options.CmdShowPack dir -> do
            possible <- directorize False (Options.PackOptions (Just dir) "")
            when possible showPack

-- Show Pack
showPack :: IO ()
showPack = do
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
targetFilter _         BP.Both   = True
targetFilter is_server BP.Client = not is_server
targetFilter is_server BP.Server = is_server

downloadBuild :: Bool -> Bool -> BP.Build -> IO ()
downloadBuild config is_server d = do
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

updateMods :: Manager -> [Current.UpdateAction] -> IO [()]
updateMods man = mapConcurrently (updateMod man)

updateMod :: Manager -> Current.UpdateAction -> IO ()
updateMod man (Current.Install m)   = downloadMod man m
updateMod man (Current.Remove v)    = removeFileColor ("mods/" ++ Current.getFilename v)
updateMod man (Current.Replace v m) = removeFileColor ("mods/" ++ Current.getFilename v) >> downloadMod man m

-- Pack Zipper
createZipPack :: String -> String -> IO ()
createZipPack "technic" packID = do
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

createZipPack "ftb" packID = do
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

packBuildURL :: String -> String -> String
packBuildURL packid buildn = baseURL ++ "packs/" ++ packid ++ "/builds/" ++ buildn

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
