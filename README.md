# [DBXFTP](https://github.com/eschnett/dbftp)

## How to create a Dropbox authorization token:

- Log in to DropBox
- Go to https://www.dropbox.com/developers/apps in web browser
- Push "Create app" button
- Choose "Dropbox API"
- Choose "App folder" (for safety)
- Choose name, e.g. "dbxftp"
- Choose account (only if you have several accounts?)
- Push "Create app" button
- Generate access token (push "Generate" button)
- Copy access token into new file "$HOME/.dbxftp.http", with prefix
  "access_token:" (i.e. if your token were "XXXXX", the file content
  would be "access_token:XXXXX". There can be a newline at the end.)

Done!
