import Data.List (sort)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Verbosity

import qualified DBXFTP

data Args = Args Verbosity Cmd
  deriving (Eq, Ord, Read, Show)

data Cmd = NoCmd
         | Put { putFiles :: [String] }
         | Ls { lsLong :: LsLong
              , lsRecursive :: LsRecursive
              , lsFiles :: [String]
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

argsLs :: Mode Cmd
argsLs = mode "ls" (Ls LsShort LsFinal []) "list directory entries"
         (flagArg addPath "path name")
         [flagNone ["long", "l"] makeLong "list metadata as well"]
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
args = modes "dbxftp" (Args Normal NoCmd) "Access DropBox from the command line"
       (remapArgs <$> [argsLs, argsPut])
       -- (flagsVerbosity makeVerbose)
  where makeVerbose :: Verbosity -> Args -> Args
        makeVerbose verb (Args _ cmd) = Args verb cmd

main :: IO ()
main =
  do putStrLn "DBXFTP"
     Args verbose cmd <- processArgs args
     case cmd of
       NoCmd -> putStrLn "No command given."
       Ls long recursive fps -> do let fps' = if null fps then [""] else fps
                                   -- add leading slash if missing
                                   -- remove trailing slashes
                                   appState <- DBXFTP.newAppState
                                   files <- DBXFTP.ls appState fps'
                                   mapM_ putStrLn (sort files)
       Put fps -> if length fps < 2
                  then putStrLn "Need at least 2 arguments"
                  else do let dst = last fps
                              fps' = init fps
                          appState <- DBXFTP.newAppState
                          DBXFTP.put appState fps' dst
     putStrLn "Done."
