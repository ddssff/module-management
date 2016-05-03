{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
-- | Replacement for debpool.
module Options
    ( Params(..)
    , initialParams
    , homeParams
    , optSpecs
    ) where

import Data.List (intercalate)
import Debian.Repo.LocalRepository (Layout(..))
import Prelude hiding (putStr, putStrLn, putChar)
import System.Console.GetOpt (ArgDescr(NoArg, ReqArg))
import System.Environment (getEnv)

import Config (ParamDescr(..))

data Params
    = Params
      { verbosity :: Int
      , rootParam :: FilePath
      , uploadSection :: Maybe FilePath
      , expire :: Bool
      , binaryOrphans :: Bool
      , cleanUp :: Bool
      , dryRun :: Bool
      , removePackages :: [String]
      , install :: Bool
      , releases :: [String]
      , aliases :: [String]
      , sections :: [String]
      , replace :: Bool
      , notifyEmail :: [String]
      , senderEmail :: Maybe String
      , verify :: Bool
      , rejectRevision :: Maybe String
      , printVersion :: Bool
      , layout :: Layout
      , sign :: Bool
      , architectures :: Maybe String
      , keyName :: Maybe String -- "Name of the pgp key with which to sign the repository."
      } deriving Show

initialParams :: Params
initialParams =
    Params
    { verbosity = 0
    , rootParam = "."
    , uploadSection = Nothing
    , expire = False
    , binaryOrphans = False
    , cleanUp = False
    , dryRun = False
    , removePackages = []
    , install = True
    , releases = []
    , aliases = []
    , sections = []
    , replace = False
    , notifyEmail = []
    , senderEmail = Nothing
    , verify = False
    , rejectRevision = Nothing
    , printVersion = False
    , layout = Pool
    , sign = True
    , architectures = Nothing
    , keyName = Nothing
    }

homeParams :: IO Params
homeParams = getEnv "HOME" >>= \ home -> return $ initialParams {rootParam = home}

optSpecs :: [ParamDescr (Params -> Params)]
optSpecs =
    [ Param ['v'] ["verbose"] ["Verbose"] (NoArg (\ p -> p {verbosity = verbosity p + 1}))
                 "Increase the amount of debugging output"
    , Param [] ["root"] ["Root"] (ReqArg (\ s p -> p {rootParam = s}) "PATH")
		 "Specify the root directory of the repository"
    , Param [] ["section"] ["Section"] (ReqArg (\ s p -> p {uploadSection = Just s}) "PATH")
		 "Force uploads to the specified section"
    , Param [] ["expire"] ["Expire"] (NoArg (\ p -> p {expire = True}))
		 "Remove all packages trumped by newer versions from the package lists."
    , Param [] ["binary-orphans"] ["Binary-Orphans"] (NoArg (\ p -> p {binaryOrphans = True}))
		 "Remove all binary packages that have no corresponding source package."
    , Param [] ["clean-up"] ["Clean-Up"] (NoArg (\ p -> p {cleanUp = True}))
		 "Move all unreferenced files in the repository to the removed directory."
    , Param ['n'] ["dry-run"] ["Dry-Run"] (NoArg (\ p -> p {dryRun = True})) "Test run, don't modify the repository.  (ONLY IMPLEMENTED FOR REMOVE)"
    , Param [] ["remove"] ["Remove"] (ReqArg (\ s p -> p {removePackages = removePackages p ++ [s]}) "DIST,SECTION,PACKAGE=VERSION")
                 "remove a particular version of a package (may be repeated.)"
    , Param ['i'] ["install"] ["Install"] (NoArg (\ p -> p {install = True}))
                 ("Scan the incoming directory and install the packages found there.\n" ++
                  "This option is automatically true if no --remove arguments are given.")
    , Param [] ["create-release"] ["Create-Release"] (ReqArg (\ s p -> p {releases = releases p ++ [s]}) "NAME")
		 ("Create any new releases and/or sections found in Distribution\n" ++
                  "and Section fields of the uploaded .changes files.")
    , Param [] ["create-alias"] ["Create-Alias"] (ReqArg (\ s p -> p {aliases = aliases p ++ [s]}) "ALIAS=RELEASE")
		 "Create an alias for an existing release"
    , Param [] ["create-section"] ["Create-Section"] (ReqArg (\ s p -> p {sections = sections p ++ [s]}) "RELEASE,SECTION")
		 "Create a new section in the given release."
    , Param [] ["replace"] ["Replace"]	(NoArg (\ p -> p {replace = True}))
                 ("Permit uploading of a package whose version is already present.\n" ++
                  "This is normally an error.")
    , Param [] ["notify-email"] ["Notify-Email"] (ReqArg (\ s p -> p {notifyEmail = notifyEmail p ++ [s]}) "USER@HOST")
                 "Email address to send notifications about success and failure of uploads."
    , Param [] ["sender-email"] ["Sender-Email"] (ReqArg (\ s p -> p {senderEmail = Just s}) "USER@HOST")
                 "Sender address for notifications."
    , Param [] ["verify"] ["Verify"] (NoArg (\ p -> p {verify = True}))
                "Verify the structure and contents of the repository."
    , Param [] ["reject-revision"] ["Reject-Revision"] (ReqArg (\ s p -> p {rejectRevision = Just s}) "STRING")
                 ("Disallow uploads of packages with this revision string.  The\n" ++
                  "autobuilder gives 'dir' targets the revision string 'none', the\n" ++
                  "option 'Reject-Revision: none' can be used to prohibit uploads of\n" ++
                  "packages built from a 'dir' target.")
    , Param [] ["version"] ["Version"] (NoArg (\ p -> p {printVersion = True}))
                 "Print the version string and exit"
    , Param [] ["layout"] ["Layout"] (ReqArg (\ s p -> p {layout = read s}) (intercalate " or " (map show ([minBound .. maxBound] :: [Layout])) ++ ", default " ++ show (layout initialParams)))
                 "Specify a default layout"
    , Param [] ["no-sign"] ["No-Sign"] (NoArg (\ p -> p {sign = False}))
                 "Do not attempt to cryptographically sign the repository, even if something changed."
    , Param [] ["sign"] ["Sign"] (NoArg (\ p -> p {sign = True}))
                 "Cryptographically sign the repository, even if nothing changed."
    , Param [] ["architectures"] ["Architectures"] (ReqArg (\ s p -> p {architectures = Just s}) "ARCH,ARCH,...")
                 ""
    , Param [] ["keyname"] ["Key-Name"] (ReqArg (\ s p -> p {keyName = Just s}) "STRING")
                 "Name of the pgp key with which to sign the repository."
{-
     , Param [] ["rebuild"] ["Rebuild"]	(NoArg (Value "Rebuild" "yes"))
     "(UNIMPLEMENTED) Reconstruct the package lists from scratch."
     , Param [] ["obsolete"] ["Obsolete"]	(NoArg (Value "Obsolete" "yes"))
     (My.consperse "\n"
      ["(UNIMPLEMENTED) Remove any package for which newer versions exist,",
       "remove any package which is not part of any dist."])
-}
    ]
