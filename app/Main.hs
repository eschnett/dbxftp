import qualified DBXFTP

main :: IO ()
main =
  do putStrLn "DBXFTP"
     appState <- DBXFTP.newAppState
     DBXFTP.put appState ["data"] "/test-20200309"
     putStrLn "Done."
