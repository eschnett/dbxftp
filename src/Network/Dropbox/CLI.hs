module Network.Dropbox.CLI (main) where

import Control.Monad.Extra
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Dropbox.API hiding (mode)
import qualified Network.Dropbox.API as Dbx
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity

import qualified DBXFTP

data Args = Args Verbosity Cmd
  deriving (Eq, Ord, Read, Show)

data Cmd = NoCmd
         | Put { putFiles :: [Path] }
         | Ls { lsLong :: LsLong
              , lsRecursive :: LsRecursive
              , lsFiles :: [Path]
              }
         deriving (Eq, Ord, Read, Show)

data LsLong = LsShort | LsLong
  deriving (Enum, Eq, Ord, Read, Show)

data LsRecursive = LsFinal | LsRecursive
  deriving (Enum, Eq, Ord, Read, Show)

embedCmd :: Cmd -> Args
embedCmd cmd = Args Normal cmd

extractCmd :: Args -> (Cmd, Cmd -> Args)
extractCmd (Args verb cmd) = (cmd, \cmd' -> Args verb cmd')

remapArgs :: Remap m => m Cmd -> m Args
remapArgs = remap embedCmd extractCmd

makeUpdate :: Update Cmd -> Update Args
makeUpdate = remapUpdate embedCmd extractCmd

--------------------------------------------------------------------------------

argsLs :: Mode Cmd
argsLs = mode "ls" (Ls LsShort LsFinal []) "list directory entries"
         (flagArg addPath "path name")
         [ flagNone ["long", "l"] makeLong "show metadata"
         , flagNone ["recursive", "r"] makeRecursive
           "recursively list subdirectories"
         ]
  where addPath :: Update Cmd
        addPath fp ls = Right $ ls { lsFiles = lsFiles ls ++ [T.pack fp] }
        makeLong :: Cmd -> Cmd
        makeLong ls = ls { lsLong = LsLong }
        makeRecursive :: Cmd -> Cmd
        makeRecursive ls = ls { lsRecursive = LsRecursive }

argsPut :: Mode Cmd
argsPut = mode "put" (Put []) "upload file or directory"
          (flagArg addPath "path name")
          []
  where addPath :: Update Cmd
        addPath fp put = Right $ put { putFiles = putFiles put ++ [T.pack fp] }

args :: Mode Args
args = modes "dbxftp" (Args Normal NoCmd) "Access DropBox from the command line"
       (remapArgs <$> [argsLs, argsPut])
       -- (flagsVerbosity makeVerbose)
  where makeVerbose :: Verbosity -> Args -> Args
        makeVerbose verb (Args _ cmd) = Args verb cmd

main :: IO ()
main =
  do putStrLn "DBXFTP: Access DropBox via the command line"
     Args verbose cmd <- processArgs args
     runCmd cmd
     putStrLn "Done."

--------------------------------------------------------------------------------

runCmd :: Cmd -> IO ()
runCmd NoCmd =
  putStrLn "No command given."

--------------------------------------------------------------------------------

runCmd (Ls long recursive fps) = mapM_ ls fps
  where
    ls :: Path -> IO ()
    ls fp =
      do mgr <- newManager
         let arg = GetMetadataArg fp
         md <- getMetadata mgr arg
         let format = if long == LsShort then prettyName else prettyInfo
         case md of
           FolderMetadata _ _ _ _
             -> do let arg = ListFolderArg fp (recursive == LsRecursive)
                   mds <- listFolder mgr arg
                   mapM_ (T.putStrLn . format) mds
           _ -> T.putStrLn (format md)
    prettyName :: Metadata -> T.Text
    prettyName NoMetadata = "<not found>"
    prettyName md = fromMaybe (name md) (pathDisplay md)
    prettyInfo :: Metadata -> T.Text
    prettyInfo md@(FileMetadata _ _ _ _ _ _ _) =
      T.intercalate " " [ typeString md
                        , T.pack $ show $ size md
                        , prettyName md
                        , symlinkTarget md]
    prettyInfo md =
      T.intercalate " " [ typeString md
                        , "-"
                        , prettyName md
                        ]
    typeString :: Metadata -> T.Text
    typeString md@(FileMetadata _ _ _ _ _ _ _) =
      case symlinkInfo md of
        Nothing -> " "
        Just _ -> "s"
    typeString (FolderMetadata _ _ _ _) = "d"
    typeString (DeletedMetadata _ _ _) = "D"
    typeString NoMetadata = "?"
    symlinkTarget :: Metadata -> T.Text
    symlinkTarget md@(FileMetadata _ _ _ _ _ _ _) =
      case symlinkInfo md of
        Nothing -> ""
        Just sym -> T.intercalate " " ["->", target sym]
    symlinkTarget _ = ""

--------------------------------------------------------------------------------

runCmd (Put fps) =
  if length fps < 2
  then putStrLn "Need at least 2 arguments"
  else do let dst = last fps
              fps' = init fps
          appState <- DBXFTP.newAppState
          -- DBXFTP.put appState fps' dst
          return ()
