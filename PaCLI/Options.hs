module PaCLI.Options (Command(..), OptionGroup(..), mainParser) where

import Options.Applicative

data OptionGroup = PackOptions { getDir :: Maybe String, getPackId :: String }
                 | DownloadOptions { getConfig :: Bool, getIsServer :: Bool }

data Command = CmdDownloadPack String OptionGroup OptionGroup
             | CmdUpdatePack OptionGroup OptionGroup
             | CmdCreateZipPack String OptionGroup
             | CmdShowPack String

-- OptionGroup parsers
packOpt :: Parser OptionGroup
packOpt = PackOptions
    <$> optional (strOption (short 'd' <> long "directory" <> metavar "DIR" <> help "Use DIR instead of Pack ID"))
    <*> argument str (metavar "PACKID")

downloadOpt :: Parser OptionGroup
downloadOpt = DownloadOptions
    <$> switch (short 'c' <> long "config" <> help "Download config.")
    <*> switch (short 's' <> long "server" <> help "Download server mods instead of client mods")

-- Command parsers
downloadPack :: Parser Command
downloadPack = CmdDownloadPack 
    <$> strOption (short 'b' <> long "build" <> metavar "BUILD" <> help "Use BUILD instead of the latest build" <> value "-1")
    <*> downloadOpt <*> packOpt

updatePack :: Parser Command
updatePack = CmdUpdatePack <$> downloadOpt <*> packOpt

createZipPack :: Parser Command
createZipPack = CmdCreateZipPack
    <$> flag "technic" "ftb" (short 'f' <> long "ftb" <> help "Use FTB Format, rather than Technic")
    <*> packOpt

showPack :: Parser Command
showPack = CmdShowPack <$> argument str (metavar "DIR")

mainParser :: Parser Command
mainParser = subparser
    (command "downloadPack" (info (helper <*> downloadPack) (progDesc "Downloads the latest build of a Pack, overwriting prior downloads of the pack and everything in the pack's folder")) <>
     command "updatePack" (info (helper <*> updatePack) (progDesc "Updates a Pack to the latest build")) <>
     command "createZip" (info (helper <*> createZipPack) (progDesc "Creates a zip pack from an existing directory")) <>
     command "showPack" (info (helper <*> showPack) (progDesc "Shows information about a downloaded pack")))
