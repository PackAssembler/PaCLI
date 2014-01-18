module PaCLI.Options (Command(..), mainParser) where

import Options.Applicative

data Command = CmdDownloadBuild Bool String
             | CmdDownloadPack Bool String
             | CmdUpdatePack Bool String
             | CmdCreateZipPack String String
             | CmdShowPack String

downloadBuild :: Parser Command
downloadBuild = CmdDownloadBuild
    <$> switch (short 'c' <> long "config" <> help "Download config.")
    <*> argument str (metavar "BUILDID")

downloadPack :: Parser Command
downloadPack = CmdDownloadPack
    <$> switch (short 'c' <> long "config" <> help "Download config.")
    <*> argument str (metavar "PACKID")

updatePack :: Parser Command
updatePack = CmdUpdatePack
    <$> switch (short 'c' <> long "config" <> help "Download config.")
    <*> argument str (metavar "PACKID")

createZipPack :: Parser Command
createZipPack = CmdCreateZipPack
    <$> flag "technic" "ftb" (short 'f' <> long "ftb" <> help "Use FTB Format, rather than Technic.")
    <*> argument str (metavar "PACKID")

showPack :: Parser Command
showPack = CmdShowPack <$> argument str (metavar "PACKID")

mainParser :: Parser Command
mainParser = subparser
    (command "downloadBuild" (info (helper <*> downloadBuild) (progDesc "Downloads a Pack Build, overwriting prior downloads of pack")) <>
     command "downloadPack" (info (helper <*> downloadPack) (progDesc "Downloads the latest build of a Pack, overwriting prior downloads of pack")) <>
     command "updatePack" (info (helper <*> updatePack) (progDesc "Updates a Pack to the latest build")) <>
     command "createZip" (info (helper <*> createZipPack) (progDesc "Creates a zip pack from an existing directory")) <>
     command "showPack" (info (helper <*> showPack) (progDesc "Shows information about a downloaded pack")))