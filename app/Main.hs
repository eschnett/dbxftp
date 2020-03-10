import qualified DBXFTP

main :: IO ()
main =
  do putStrLn "DBXFTP"
     appState <- DBXFTP.newAppState
     -- DBXFTP.ls appState "/test-20200309"
     DBXFTP.put appState ["/tmp/data"] "/test-20200309"
     putStrLn "Done."
