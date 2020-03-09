import qualified DBXFTP

main :: IO ()
main =
  do appState <- DBXFTP.newAppState
     DBXFTP.put appState ["/Users/eschnett/src/hs/dbxftp/data"] "/test-20200307"
