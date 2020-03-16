module Network.Dropbox.CLI (main) where

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Network.Dropbox.API hiding (mode)
import Network.Dropbox.Filesystem hiding (contentHash)
import Prelude
import Streamly
import qualified Streamly.Prelude as S
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity
import System.FilePath
import System.Posix

data Args = Args Verbosity Cmd
  deriving (Eq, Ord, Read, Show)

data Cmd = NoCmd
         | Ls { lsLong :: LsLong
              , lsRecursive :: LsRecursive
              , lsFiles :: [String]
              }
         | Put { putFiles :: [String] }
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
        addPath fp ls = Right $ ls { lsFiles = lsFiles ls ++ [fp] }
        makeLong :: Cmd -> Cmd
        makeLong ls = ls { lsLong = LsLong }
        makeRecursive :: Cmd -> Cmd
        makeRecursive ls = ls { lsRecursive = LsRecursive }

argsPut :: Mode Cmd
argsPut = mode "put" (Put []) "upload file or directory"
          (flagArg addPath "path name")
          []
  where addPath :: Update Cmd
        addPath fp put = Right $ put { putFiles = putFiles put ++ [fp] }

args :: Mode Args
args = modes "dbxftp" (Args Normal NoCmd)
       "An ftp-like command line interface to DropBox"
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
runCmd NoCmd = putStrLn "No command given."
runCmd (Ls long recursive fps) = ls long recursive $ fmap T.pack fps
runCmd (Put fps)
  | null fps = putStrLn "Need at least 1 argument"
  | otherwise = let (srcs, dst) = (init fps, T.pack $ last fps)
                in put srcs dst

--------------------------------------------------------------------------------

ls :: LsLong -> LsRecursive -> [Path] -> IO ()
ls long recursive fps = do
  mgr <- liftIO newManager
  (S.mapM_ T.putStrLn :: Serial T.Text -> IO ())
    $ (aheadly :: Ahead T.Text -> Serial T.Text)
    $ (S.concatMap (ls1 mgr) :: Ahead Path -> Ahead T.Text)
    |$ (S.fromList fps :: Ahead Path)
  where
    ls1 :: Manager -> Path -> Ahead T.Text
    ls1 mgr fp = do
      let arg = GetMetadataArg fp
      md <- liftIO
            $ handleJust (\case DbxNotFoundException{} -> Just ()
                                _ -> Nothing)
                         (\_ -> do putStrLn $ show fp ++ ": not found"
                                   return NoMetadata)
            $ getMetadata mgr arg
      case md of
        NoMetadata -> S.nil
        FolderMetadata{} ->
          let arg = ListFolderArg fp (recursive == LsRecursive)
          in format <$> serially (listFolder mgr arg)
        _ -> S.yield $ format md
    format :: Metadata -> T.Text
    format = case long of
      LsShort -> formatName
      LsLong -> formatInfo
    formatName :: Metadata -> T.Text
    formatName NoMetadata = "<not found>"
    formatName md = fromMaybe (name md) (pathDisplay md)
    formatInfo :: Metadata -> T.Text
    formatInfo md@FileMetadata{} =
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
    typeString md@FileMetadata{} =
      case symlinkInfo md of
        Nothing -> "-"
        Just _ -> "s"
    typeString FolderMetadata{} = "d"
    typeString DeletedMetadata{} = "D"
    typeString NoMetadata = "?"
    symlinkTarget :: Metadata -> T.Text
    symlinkTarget md@FileMetadata{} =
      case symlinkInfo md of
        Nothing -> ""
        Just sym -> T.append "-> " (target sym)
    symlinkTarget _ = ""

--------------------------------------------------------------------------------

put :: [FilePath] -> Path -> IO ()
put fps dst = do
  let srcs = S.fromList fps :: Async FilePath
  fmgr <- liftIO newFileManager :: IO FileManager
  let srcdsts = listDirsRec fmgr dst |$ srcs
        :: Async (FilePath, FileStatus, Path)
  -- liftIO $ putStrLn "Locals:"
  -- S.drain $ asyncly $ S.mapM print |$ srcdsts
  mgr <- newManager
  let dsts = listFolder mgr (ListFolderArg dst True) :: Serial Metadata
  -- liftIO $ putStrLn "Remotes:"
  -- S.drain $ S.mapM print |$ dsts
  dsts' <- S.toList $ S.map makePair |$ dsts :: IO [(Path, Metadata)]
  let dstmap = H.fromList dsts' :: H.HashMap Path Metadata
  -- liftIO $ putStrLn "Remotes:"
  -- liftIO $ print dstmap
  let uploads = S.filterM (needUploadFile fmgr mgr dstmap)
                |$ srcdsts
  -- liftIO $ putStrLn "Uploads:"
  -- S.drain $ asyncly $ S.mapM print |$ uploads
  let ress = uploadFiles fmgr mgr
             |$ S.map makeUploadFileArg
             |$ uploads
  -- liftIO $ putStrLn "Results:"
  -- S.drain $ asyncly $ S.mapM print |$ ress
  S.drain $ asyncly ress
  where
    listDirsRec :: FileManager
                -> Path
                -> Async FilePath
                -> Async (FilePath, FileStatus, Path)
    listDirsRec fmgr dst = S.concatMap (listDirRec fmgr dst)
    listDirRec :: FileManager
               -> Path
               -> FilePath
               -> Async (FilePath, FileStatus, Path)
    listDirRec fmgr dst src = do
      fs <- liftIO $ fileStatus fmgr src
      if isDirectory fs
        then S.concatMap (listDirRec1 fmgr dst src) |$ listDir1 fmgr src
        else S.yield (src, fs, dst)
    listDir1 :: FileManager -> FilePath -> Async FilePath
    listDir1 fmgr fp = serially $ listDir fmgr fp
    listDirRec1 :: FileManager
                -> Path -> FilePath
                -> FilePath -> Async (FilePath, FileStatus, Path)
    listDirRec1 fmgr dst src fp =
      listDirRec fmgr (appendPath dst fp) (src </> fp)
    appendPath :: Path -> FilePath -> Path
    appendPath p fp = T.concat [p, "/", T.pack $ takeFileName fp]
    makePair :: Metadata -> (Path, Metadata)
    makePair NoMetadata = ("<not found>", NoMetadata)
    makePair md = (fromMaybe (name md) (pathDisplay md), md)
    needUploadFile :: FileManager -> Manager -> H.HashMap Path Metadata
                   -> (FilePath, FileStatus, Path) -> IO Bool
    needUploadFile fmgr mgr dstmap (fp, fs, p) = do
      case H.lookup p dstmap of
        Nothing -> do putStrLn $ ("Uploading " ++ show p
                                  ++ " (remote does not exist)")
                      return True
        Just md ->
          if size md /= fromIntegral (fileSize fs)
          then do putStrLn $ "Uploading " ++ show p ++ " (remote size differs)"
                  return True
          else do ContentHash hash <- fileContentHash fmgr fp
                  let hashDiffers = T.encodeUtf8 (contentHash md) /= hash
                  if hashDiffers
                    then putStrLn $ "Uploading " ++ show p ++ " (hash differs)"
                    else putStrLn $ "Skipping " ++ show p
                  return hashDiffers
    makeUploadFileArg :: (FilePath, FileStatus, Path) -> UploadFileArg
    makeUploadFileArg (fp, fs, p) = let mode = Overwrite
                                        autorename = False
                                        mute = False
                                    in UploadFileArg fp p mode autorename mute
