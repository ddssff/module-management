-- | Set up or verify SSH access to a remove machine.
module Debian.Repo.Prelude.SSH
    ( sshVerify
    , sshExportDeprecated
    , sshCopy
    ) where

import System.Directory
import System.Posix.User
import System.Environment
import System.Exit
import System.IO
import System.Process (system, readProcessWithExitCode, showCommandForUser)

-- |Set up access to destination (user\@host).
sshExportDeprecated :: String -> Maybe Int -> IO (Either String ())
sshExportDeprecated dest port =
    generatePublicKey >>=
    either (return . Left) (testAccess dest port) >>=
    either (return . Left) (openAccess dest port)

-- parseURI "ssh://dsf@server:22"
-- URI {uriScheme = "ssh:", uriAuthority = Just (URIAuth {uriUserInfo = "dsf@", uriRegName = "server", uriPort = ":22"}), uriPath = "", uriQuery = "", uriFragment = ""}

-- |Make sure there is a public key for the local account
generatePublicKey :: IO (Either String FilePath)
generatePublicKey =
    do user <- getEffectiveUserID
       home <- getUserEntryForID user >>= return . homeDirectory
       let cmd = "yes '' | ssh-keygen -t rsa 2>&1 >/dev/null"
       let keypath = home ++ "/.ssh/id_rsa.pub"
       exists <- doesFileExist keypath
       case exists of
         True -> return . Right $ keypath
         False ->
             do hPutStrLn stderr $ "generatePublicKey " ++ " -> " ++ keypath
                code <- system cmd
                case code of
                  ExitFailure n ->
                      return . Left $ "Failure: " ++ show cmd ++ " -> " ++ show n
                  _ -> return . Right $ keypath

-- |See if we already have access to the destination (user\@host).
sshVerify :: String -> Maybe Int -> IO (Either String ())
sshVerify dest port =
    do r@(result, _out, _err) <- readProcessWithExitCode cmd args ""
       case result of
         ExitSuccess -> return (Right ())		-- We do
         ExitFailure _ ->
             hPutStrLn stderr (showCommandForUser cmd args ++ " -> " ++ show r) >> return (Left (show r))	-- We do not
    where
      cmd = "ssh"
      args = (["-o", "PreferredAuthentications hostbased,publickey"] ++ maybe [] (\ n -> ["-p", show n]) port ++ [dest, "pwd"])
      _sshTestCmd dest' port' =
          ("ssh -o 'PreferredAuthentications hostbased,publickey' " ++
           (maybe "" (("-p " ++) . show) port') ++ " " ++ show dest' ++ " pwd > /dev/null && exit 0")

testAccess :: String -> Maybe Int -> FilePath -> IO (Either String (Maybe FilePath))
testAccess dest port keypath =
    sshVerify dest port >>= return . either Left (\ () -> Right (Just keypath))

-- |Try to set up the keys so we have access to the account.  I don't
-- think we use this any more, and I don't think you should either.
openAccess :: String -> Maybe Int -> Maybe FilePath -> IO (Either String ())
openAccess _ _ Nothing = return . Right $ ()
openAccess dest port (Just keypath) =
    do hPutStrLn stderr $ "openAccess " ++ show dest ++ " " ++ show port ++ " " ++ show keypath
       let args = maybe [] (\ x -> ["-p", show x]) port  ++ [show dest, sshOpenRemoteCmd]
       (code, out, err) <- readFile keypath >>= readProcessWithExitCode "ssh" args
       case code of
         ExitFailure n -> return . Left $ "Failure: " ++ showCommandForUser "ssh" args ++ " -> " ++ show n ++
	                                  "\n\nstdout: " ++ out ++ "\n\nstderr: " ++ err
         _ -> return . Right $ ()
    where
      sshOpenRemoteCmd =
          ("chmod g-w . && " ++				-- Ssh will not work if the permissions aren't just so
           "chmod o-rwx . && " ++
           "mkdir -p .ssh && " ++
           "chmod 700 .ssh && " ++
           "cat >> .ssh/authorized_keys2 && " ++	-- Add the key to the authorized key list
           "chmod 600 .ssh/authorized_keys2")

-- This used to be main.
{-
test =
    getDest >>=
    either (return . Left) (uncurry sshExport) >>=
    either (error . show) (const . exitWith $ ExitSuccess)

-- |Get the destination account info from the command line
getDest :: IO (Either String (String, Maybe Int))
getDest =
    getArgs >>= checkArgs
    where checkArgs [dest] =
              return $ case parseURI ("ssh://" ++ dest) of
                         Just (URI {uriAuthority = Just (URIAuth {uriUserInfo = user, uriRegName = host, uriPort = ""})}) ->
                             Right (user ++ host, Nothing)
                         Just (URI {uriAuthority = Just (URIAuth {uriUserInfo = user, uriRegName = host, uriPort = port})}) ->
                             case reads (dropWhile (== ':') port) :: [(Int, String)] of
                               [] -> Left $ "Invalid destination: " ++ dest ++ " (" ++ port ++ ")"
                               ((n, _) : _) -> Right (user ++ host, Just n)
                         _ -> Left $ "Invalid destination: " ++ dest
          checkArgs args = return . Left $ "Usage: sshexport user@host"
-}

-- |Copy the ssh configuration from $HOME to the \/root directory of a
-- changeroot.
sshCopy :: FilePath -> IO ExitCode
sshCopy root =
    do exists <- doesDirectoryExist "~/.ssh"
       home <- getEnv "HOME"
       case exists of
         True -> system ("mkdir -p " ++ root ++ "/root && " ++
                         "rsync -aHxSpDt --delete " ++ home ++ "/.ssh/ " ++ root ++ "/root/.ssh && " ++
                         "chown -R root.root " ++ root ++ "/root/.ssh")
         False -> system "mkdir -p /root/.ssh"
