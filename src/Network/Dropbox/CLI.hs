module Network.Dropbox.CLI (main) where

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Combinators
import Data.List hiding (concat, concatMap, map)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Network.Dropbox.API hiding (mode)
import Prelude hiding (concat, concatMap, map, mapM, mapM_, unlines)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity

-- import qualified DBXFTP

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
main = do
  putStrLn "DBXFTP: Access DropBox via the command line"
  Args verbose cmd <- processArgs args
  runCmd cmd
  putStrLn "Done."

--------------------------------------------------------------------------------

runCmd :: Cmd -> IO ()
runCmd NoCmd =
  putStrLn "No command given."

--------------------------------------------------------------------------------

runCmd (Ls long recursive fps) = do
  mgr <- liftIO newManager
  runConduit $ yieldMany fps .| ls mgr .| map encode .| stdout
  where
    ls :: Manager -> ConduitT Path T.Text IO ()
    ls mgr = awaitForever \fp -> do
      let arg = GetMetadataArg fp
      md <- liftIO $ getMetadata mgr arg
      case md of
        FolderMetadata {} ->
          let arg = ListFolderArg fp (recursive == LsRecursive)
          in map (const ()) .| listFolder mgr arg .| map format
        _ -> yield $ format md
    encode :: T.Text -> BS.ByteString
    encode t = T.encodeUtf8 (T.append t "\n")
    format :: Metadata -> T.Text
    format = case long of
      LsShort -> formatName
      LsLong -> formatInfo
    formatName :: Metadata -> T.Text
    formatName NoMetadata = "<not found>"
    formatName md = fromMaybe (name md) (pathDisplay md)
    formatInfo :: Metadata -> T.Text
    formatInfo md@(FileMetadata {}) =
      T.intercalate " " [ typeString md
                        , T.pack $ show $ size md
                        , formatName md
                        , symlinkTarget md]
    formatInfo md =
      T.intercalate " " [ typeString md
                        , "-"
                        , formatName md
                        ]
    typeString :: Metadata -> T.Text
    typeString md@(FileMetadata {}) =
      case symlinkInfo md of
        Nothing -> "-"
        Just _ -> "s"
    typeString (FolderMetadata {}) = "d"
    typeString (DeletedMetadata {}) = "D"
    typeString NoMetadata = "?"
    symlinkTarget :: Metadata -> T.Text
    symlinkTarget md@(FileMetadata {}) =
      case symlinkInfo md of
        Nothing -> ""
        Just sym -> T.append "-> " (target sym)
    symlinkTarget _ = ""

--------------------------------------------------------------------------------

-- runCmd (Put fps) =
--   if length fps < 2
--   then putStrLn "Need at least 2 arguments"
--   else do let dst = last fps
--               fps' = init fps
--           appState <- DBXFTP.newAppState
--           DBXFTP.put appState fps' dst
