{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- |The 'Config' module implements both command line and configuration
-- file option handling.  It converts the command line parameters into
-- Flag objects, and then expands any Include, Use, or Config flags.
-- Every command line parameter has an equivalent format that can
-- appear in the configuration file.  Using Debian conventions, a
-- command line option such as
--
--    --some-option=value
--
-- could appear in a configuration using this syntax:
--
--    Some-Option: value
--
-- This is the format used in Debian Control files, and is similar to
-- the format described in RFC-922.  Note that a value in a control
-- file can be continued onto multiple lines by prefixing the extra
-- lines with whitespace, as described here:
-- <http://www.debian.org/doc/debian-policy/ch-controlfields.html>
--
-- See the documentation of the Flag datatype below for a description
-- of the features this module supports.
--
-- Author: David Fox <ddssff@gmail.com>
module Config
    ( Flag(..)
    -- , seedFlags
    , optBaseSpecs
    , computeConfig
    , configPath
    , findValues
    , findValue
    , findBool
    -- , values
    , ParamDescr(..)
    , option
    , Config.usageInfo
    ) where

import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Debian.Control
import System.Console.GetOpt
import System.Directory
import System.Environment as Environment
import System.IO
import Text.Regex

-- | The command line arguments are converted to a list of 'Flag'
-- objects, and this list is then expanded by the 'computeConfig'
-- function and the result (due to the operation of 'Use') is a list
-- of Flag lists.
data Flag
    = Include FilePath |
      -- ^ (Command line equivalents: @--include@ or @--config@.)  The FilePath
      -- value is a file or directory containing configuration information.
      -- If no 'Include' option is given, default locations will be used as
      -- described in the 'computeConfig' function.
      Name String |
      -- ^ (No command line equivalent).  This flag is used to
      -- assign a name to a paragraph, which can then be referred to by a
      -- 'Use' flag.
      Use [String] |
      -- ^ (Command line equivalent: @--use@.)  The 'Use' flag is
      -- refers to one or more 'Name'ed paragraphs.  Referring to a
      -- single named paragraph causes the parameters in that
      -- paragraph to be added to the set of parameters we are
      -- computing.  If several paragraphs are named, a copy the
      -- current set of parameters is created for each named
      -- paragraph, and the result is several sets of parameters.
      -- This is what causes the result of the 'computeConfig' function
      -- to be a list of 'Flag' lists.
      Let String String |
      -- ^ (Command line equivalent: @--let name=value@.)  Define a
      -- macro @name@, occurrences of @${name}@ will expand to @value@.
      Value String String
      -- ^ (No command line equivalent.)  Any other named parameter becomes
      -- a 'Value' flag.
      deriving Eq

isUse :: Flag -> Bool
isUse (Use _) = True
isUse _ = False

-- |Display a flag in (pseudo) RFC-922 format: @Name: Value@
instance Show Flag where
    show (Include x) = "Include: " ++ x
    show (Name x) = "Name: " ++ x
    show (Use xs) = "Use: " ++ consperse " " xs
    show (Let name value) = "Let:" ++ name ++ "=" ++ value
    show (Value name value) = name ++ ": " ++ value

instance Read Flag where
    readsPrec _ =
        let re = mkRegex "^([^ \t:]+):([^\n]*(\n[ \t].*)*)($|\n)" in
        (\ s ->
             case matchRegexAll re s of
               Just (_, _, after, [name, value, _, _]) ->
                   case name of
                     "Include" -> [(Include (stripWS value), after)]
                     "Config"  -> [(Include (stripWS value), after)]
                     "Name" -> [(Name (stripWS value), after)]
                     "Use" -> [(Use (words value), after)]
                     "Let" -> [(parseLet value, after)]
                     _ -> [(Value (stripWS name) (stripWS value), after)]
               _ -> error ("Parse error reading flag: '" ++ s ++ "'"))

{-
-- | Convert a list of command line arguments into a set of flags.
-- These seed flags are later expanded by applying the Name\/Use
-- mechanism to the information loaded from the configuration file or
-- directory.  The appName argument is used to construct the usage
-- string when invalid arguments are given.
seedFlags :: String -> [ParamDescr a] -> [String] -> [Flag]
seedFlags appName options args =
    -- Convert the command line arguments to flags.  Any arguments
    -- not recognized by getOpt is treated as implicit "--use"
    -- parameter.
    case getOpt Permute (map option customizedOptions) args of
      (o, [], []) -> o
      (o, extra, []) -> (o ++ [Use extra])
      (_, _, errs) -> error (concat errs ++ Debian.Config.usageInfo header customizedOptions)
    where
      customizedOptions = mergeSpecs appName options
      header = "Usage: " ++ appName ++ " [OPTION...]"
-}

-- |Return the configuration information computed from a set of seed
-- flags and the configuration files.
-- Example:
--
-- > > computeConfig "autobuilder" Nothing [Include "/home/build/.autobuilder.d"]
-- > [[Name "common",Value "Vendor-Tag" "cnr",Value "Release-Tag" "1", ...
computeConfig :: Int			-- ^ Preliminary verbosity level, before we have obtained the verbosity parameter
              -> String			-- ^ The application name, used to construct candidate configuration file names.
              -> [Flag]			-- ^ The set of flags read from the configuration
              -> ([[Flag]] -> [[Flag]])	-- ^ Final preparation of the configuration file contents
              -> IO [[Flag]]		-- ^ The result is a list of flag lists.  See 'Use' for an explanation of how you would get
					-- more than one flag list here.
computeConfig verbosity appName commandLineFlags prepare =
    do when (verbosity > 2) (hPutStrLn stderr $ "computeConfig: commandLineFlags=" ++ show commandLineFlags)
       -- Compute the configuration file path and then load and expand it.
       defaultIncludes <- defaultConfigPaths appName
       --hPutStrLn stderr $ "defaultIncludes " ++ show defaultIncludes
       configFlags <- configPath verbosity (commandLineFlags ++ defaultIncludes) >>=
                      tryPaths . maybeToList >>= expandIncludes >>= return . prepare
       when (verbosity > 2) (hPutStrLn stderr $ "computeConfig: configFlags=" ++ show commandLineFlags)
       -- Expand the command line flags using the Use/Name mechanism, and then expand the lets.
       expandSections [commandLineFlags] configFlags >>= return . map (expandLets . checkLets)
    where
      -- Load a list of configuration files.
      expandIncludes :: [[Flag]] -> IO [[Flag]]
      expandIncludes flags =
          do when (verbosity > 2) (hPutStrLn stderr $ "expandIncludes: flags=" ++ show flags)
             -- ePut ("flags: " ++ show flags)
             let paths = concat (map includes flags)
             when (verbosity > 2) (hPutStrLn stderr $ "expandIncludes: paths=" ++ show paths)
             -- ePut (concat (map (("Include: " ++) . (++ "\n")) paths))
             case paths of
               [] -> return flags
               _ ->
                   do newflags <- tryPaths paths
                      when (verbosity > 2) (hPutStrLn stderr ("expandIncludes: newflags=" ++ show newflags))
                      return (flags ++ newflags)
      checkLets :: [Flag] -> [Flag]
      checkLets flags =
          concat (map (checkLetGroup . nub) (groupBy sameLetName (sortBy compareLetName flags)))
          where
            compareLetName (Let a _) (Let b _) = compare a b
            compareLetName (Let _ _) _ = LT
            compareLetName _ (Let _ _) = GT
            compareLetName _ _ = EQ
            sameLetName a b = compareLetName a b == EQ
            checkLetGroup :: [Flag] -> [Flag]
            checkLetGroup x@[Let _ _] = x
            checkLetGroup xs@(Let a _ : _) = error $ "Multiple definitions of " ++ a ++ ": " ++ show xs
            checkLetGroup xs = xs
      -- Find the Let flags and use them to perform expansions on
      -- all the flags in the list.
      expandLets :: [Flag] -> [Flag]
      expandLets flags = map expand flags
          where
            expand (Include path) = Include (expand' path)
            expand (Name s) = Name (expand' s)
            expand (Use l) = Use (map expand' l)
            -- Not expanding let names or parameter names.
            expand (Let name value) = Let (expand' name) (expand' value)
            expand (Value name value) = Value (expand' name) (expand' value)
            -- Do expansions of a string.  S is a variable name followed by "}"
            -- and maybe some more text.
            expand' ('$' : '{' : s) = expand'' s alist
            expand' (x : s) = x : expand' s
            expand' "" = ""
            -- Try each expansion in turn on the name at the beginning of S.
            expand'' s [] = "${" ++ expand' s ++ "}"	-- no expansion possible
            expand'' s ((name, value) : etc) =
                if isPrefixOf (name ++ "}") s then
                    value ++ expand' (drop (length name + 1) s) else
                    expand'' s etc
            alist = lets flags
      -- Select the --include arguments from a list of flags
      includes (Include path : etc) = path : includes etc
      includes (_ : etc) = includes etc
      includes [] = []
      -- Select the --let arguments from a flag list and return an alist
      lets :: [Flag] -> [(String, String)]
      lets (Let name value : etc) = (name, value) : lets etc
      lets (_ : etc) = lets etc
      lets [] = []

-- |Find the configuration file or directory we will use, if any.
-- This the first file or directory that exists among these:
--
-- * The 'Include' elements from the list of flags 
--
-- * @$HOME\/.\<appName\>.d@
--
-- * @$HOME\/.\<appName\>.conf@
--
-- * @\/etc\/\<appName\>.d@
--
-- * @\/etc\/\<appName\>.conf@
configPath :: Int -> [Flag] -> IO (Maybe FilePath)
configPath verbosity flags =
    do when (verbosity > 1) (hPutStrLn stderr ("Seed flags: " ++ show flags))
       path <- filterM exists (findIncludes flags) >>= return . listToMaybe
       when (verbosity > 0) (hPutStrLn stderr ("Configuration file: " ++ show path))
       return path
    where
      exists path =
          do fileExists <- doesFileExist path
             dirExists <- doesDirectoryExist path
             return (fileExists || dirExists)
      -- Find the value of the --include and (synonymous) --config flags
      findIncludes [] = []
      findIncludes (Include path : etc) = path : findIncludes etc
      findIncludes (_ : etc) = findIncludes etc

-- | Return a list of paths where we might find a configuration file
-- or directory.
defaultConfigPaths :: String -> IO [Flag]
defaultConfigPaths appName =
    do paths1 <- try (getEnv "HOME") >>=
                 return . either (\ (_ :: SomeException) -> []) 
                                 (\ home -> [home ++ "/." ++ appName ++ ".d",
                                             home ++ "/." ++ appName ++ ".conf"])
       let paths2 = ["/etc/" ++ appName ++ ".d",
                     "/etc/" ++ appName ++ ".conf"]
       return $ map Include (paths1 ++ paths2)

-- Load a list of configuration files.
tryPaths :: [FilePath] -> IO [[Flag]]
tryPaths paths = do
    --hPutStrLn stderr ("tryPaths " ++ show paths)
    flags <- mapM tryPath paths >>= return . mergeControls >>= return . flagsOfControl
    return flags
    where
      -- Each paragraph of the control file becomes a list of flags
      flagsOfControl (Control paragraphs) = map (\ (Paragraph fields) -> catMaybes (map flagOfField fields)) paragraphs
      flagOfField (Field (name, value)) = Just . read $ name ++ ": " ++ value
      flagOfField _ = Nothing
      tryPath path =
	  do
            --hPutStrLn stderr (" tryPath " ++ show path)
            isDir <- doesDirectoryExist path
            case isDir of
              False -> do
                  try (parseControlFromFile path) >>=
                      either (\ (e :: SomeException) -> error . ((path ++ ": ") ++) . show $ e) return >>=
	              either (\ e -> error . ((path ++ ": ") ++) . show $ e) return
              True -> do
                   getDirectoryContents path >>=
                      return . map ((path ++ "/") ++) . sort . filter isConfigPart >>=
                      mapM tryPath >>=
                      return . mergeControls
      isConfigPart "" = False
      isConfigPart s | {- isDigit (head s) && -} head s /= '.' && last s /= '~' && s /= "_darcs" = True
      isConfigPart _ = False

-- |Expand occurrences of @--use@ in a list of flag lists.  The
-- expansion is appended (not prepended) to the list of flags so that
-- the command line and global options remain at the beginning of the
-- list.
expandSections :: [[Flag]] -> [[Flag]] -> IO [[Flag]]
expandSections toExpand expansions =
    do
      expanded <- mapM (expandSection []) toExpand
      return (concat expanded)
    where
      expandSection :: [String] -> [Flag] -> IO [[Flag]]
      expandSection stack xs =
          do
            -- ePut ("stack: " ++ show stack)
            -- ePut ("toExpand: " ++ show xs)
            -- ePut ("expansions: " ++ show expansions)
            let (useFlags, otherFlags) = partition isUse xs
            -- ePut ("useFlags: " ++ show useFlags)
            let sequences = map getNames useFlags
            -- ePut ("sequences: " ++ show sequences)
            -- A sequence of name lists
            let (sequences' :: [[String]]) = cartesianProduct sequences
            -- ePut ("sequence: " ++ show sequence)
            -- map (elem stack) (concat sequence)
            let (newstack :: [String]) = nub $ stack ++ concat sequences'
            -- Remove any names which are already being expanded,
            -- either because of a cycle in the Use->Name graph or
            -- because an element is reachable by multiple paths.
            let (sequence''' :: [[String]]) = map (\ s -> filter (\ e -> not (elem e stack)) s) sequences'
            -- ePut ("newstack: " ++ show newstack)
            -- A sequence of flag lists
            let (sequence' :: [[[Flag]]]) = map (expandNames stack) sequence'''
            -- ePut ("sequence': " ++ show sequence')
            case sequence' of
              [] ->
                  return [otherFlags]
              _ ->
                  do
                    let sequence'' = map (otherFlags ++) (map concat sequence')
                    -- ePut ("sequence'': " ++ show sequence'')
                    result <- mapM (expandSection newstack) sequence''
                    -- ePut ("result: " ++ show result)
                    return (concat result)

      getNames (Use xs) = xs
      getNames _ = []

      -- FIXME: use the stack to prevent infinite recursion
      expandNames stack names' =
          map (expandName stack) names'
      expandName _stack name =
          maybe err id (find  (elem (Name name)) expansions)
          where
            err = error ("Configuration file section '" ++ name ++ "' not found.\nAvailable:\n  " ++ 
                         (consperse "\n  " . map show . concat . map (filter isName) $ expansions))
            isName (Name _) = True
            isName _ = False

--ePut = hPutStrLn stderr

-- |Command line option specifications.  The caller passes in a list of
-- options, and three additional standard options are added here:
-- --config <path> - specify the path to a configuration file
-- --include <path> - pull in options from a file
-- --use 'name1 name2 ...' - pull in some named sections
{-
mergeSpecs :: String -> [ParamDescr a] -> [ParamDescr a]
mergeSpecs appName specs =
    specs ++ optBaseSpecs appName
-}

optBaseSpecs :: String -> [ParamDescr Flag]
optBaseSpecs appName =
    [ Param { shortOpts = ['c']
            , longOpts = ["config","include"]
            , argDescr = (ReqArg Include "PATH")
            , names = ["Config", "Include"]
            , description =
                (consperse "\n"
                 ["Location of the configuration file or directory to",
                  "suppliment the command line arguments.  This option",
                  "may be given multiple times, but only the first",
                  "one that exists is used.  Furthermore, if none are supplied or",
                  "none of those supplied exist, four additional paths are tried,",
                  "in this order: '/etc/" ++ appName ++ ".d', '/etc/" ++ appName ++ ".conf',",
                  "'$HOME/." ++ appName ++ ".d', and '$HOME/." ++ appName ++ ".conf'.",
                  "If the configuration path specifies a directory all the files",
                  "in the directory that begin with digits are read in lexical",
                  "order and merged.  If it is a regular file, it is read and",
                  "the result is used."]) }
    , Param { shortOpts = []
            , longOpts = ["use"]
            , argDescr = (ReqArg (Use . words) "NAME")
            , names = ["Use"]
            , description =
                (consperse "\n"
                 ["When a Use: NAME parameter appears the configuration parameters",
                  "are searched for a configuration section containing Name: NAME,",
                  "and the values in that section are substituted for the occurrence",
                  "of Use.  This expansion is performed repeatedly until no more Use",
                  "parameters are left.  If an argument appears without an associated",
                  "flag, it is treated as an implied Use."]) }
    , Param { shortOpts = []
            , longOpts = ["let"]
            , argDescr = (ReqArg parseLet "SYM=VAL")
            , names = ["Let"]
            , description =
                "define SYM to be VAL, ${SYM} will be expanded to VAL wherever it appears." }
    ]

-- |Return all values of a string paramter in a flag list.
findValues :: [Flag] -> String -> [String]
findValues (Value name value : etc) sought | name == sought = value : (findValues etc sought)
findValues (_ : etc) sought = findValues etc sought
findValues [] _ = []

-- |Return the value of a string paramter in a flag list.
findValue :: [Flag] -> String -> Maybe String
findValue flags sought =
    case catMaybes . map matchName $ flags of
      [] -> Nothing
      x -> Just . last $ x
    where
      matchName (Value name value) = if name == sought then Just value else Nothing
      matchName _ = Nothing

-- |Return the value of a boolean paramter in a flag list.
findBool :: [Flag] -> String -> Bool
findBool flags sought = maybe False (\ _ -> True) (findValue flags sought)

parseLet :: String -> Flag
parseLet s =
    case matchRegex re s of
      Just [a, _, b, _] -> Let a b
      _ -> error ("Invalid define: " ++ s)
    where
      re = mkRegex ("^" ++ mw ++ nwn ++ mw ++ "=" ++ mw ++ nwn ++ mw ++ "$")
      nwn = "([^ \t](.*[^ \t])?)"	-- nonwhite, white, nonwhite
      mw = "[ \t]*"			-- maybe white

-- |The mighty consperse function
consperse :: [a] -> [[a]] -> [a]
consperse sep items = concat (intersperse sep items)

-- |cartesianProduct [[1,2,3], [4,5],[6]] -> [[1,4,6],[1,5,6],[2,4,6],[2,5,6],[3,4,6],[3,5,6]]
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = []
cartesianProduct [xs] = map (: []) xs
cartesianProduct (xs : yss) = distribute xs (cartesianProduct yss)
distribute :: [a] -> [[a]] -> [[a]]
distribute xs yss = concat (map (\ x -> map (x :) yss) xs)

-- Example:
--
-- let (optSpecs :: [OptDescr Flag]) =
--     [Option [] ["verbose"]	(ReqArg (Value "Verbose") "NUMBER")
--	"Specify the amount of debugging output",
--	Option ['n'] ["dry-run"]	(NoArg (Value "Dry-Run" "yes"))
--	"Test run, don't modify the repository."]
-- flags <- computeConfig "myapp" optSpecs Nothing >>= return . head
-- let dryRun = findBool flags "Dry-Run"
--     verbose = maybe 0 read (findValue flags "Verbose")
--

-- When this is executed, it will load either the configuration file
-- ~/.myapp.conf, /etc/myapp.conf, or some configuration file
-- specified using the --config command line app.  The top section of
-- the configuration file will be merged with the command line flags.
-- Then those flags are expanded using the rules described in the
-- definition of the Flag datatype, and the result is returned.

-- |This is basically an augmented version of the OptDescr type from
-- System.Console.GetOpt.  It adds a list of parameter names which are
-- used in the configuration file, and a function to retrieve the
-- available values for the parameter.
data ParamDescr a
    = Param { --option :: OptDescr Flag
              shortOpts :: [Char]
	    , longOpts :: [String]
            , names :: [String]
	    , argDescr :: (ArgDescr a)
	    , description :: String
            }

option :: ParamDescr a -> OptDescr a
option p =
    Option (shortOpts p) (longOpts p) (argDescr p) (description p)

-- Modified version of usageInfo.

data DescrLine
    = Opt { long :: String
          , short :: String
          , param :: String }
    | Text String

-- |Modified version of System.Console.GetOpt.usageInfo, avoids printing
-- such wide lines.
usageInfo :: String		-- header
          -> [ParamDescr a]	-- parameter descriptor
          -> String		-- nicely formatted decription of options
usageInfo header params = unlines (header:table)
   where xs = concatMap fmtOpt params
         ssl = foldl max 0 (map ss xs)
         lsl = foldl max 0 (map ls xs)
         ss (Opt {short = s}) = length s
         ss _ = 0
         ls (Opt {long = s}) = length s
         ls _ = 0
         table = map fmtLine (legend ++ xs)
         fmtLine (Text s) = "    " ++ s
         fmtLine (Opt {long = lopts, short = sopts, param = ps}) =
             "  " ++
             flushLeft lsl lopts ++ "  " ++
             flushLeft ssl sopts ++ "  " ++
             ps
         flushLeft n x = take n (x ++ repeat ' ')
         legend = [Opt {long = "Long option", short = "Short option", param = "Config file parameter"},
                   Opt {long = "-----------", short = "------------", param = "---------------------"}]

fmtOpt :: ParamDescr a -> [DescrLine]
fmtOpt paramDescr =
   let Option sos los ad descr = option paramDescr in
   let ds = [Text ""] ++ map Text (lines descr) ++ [Text ""]
       ss = map (fmtShort ad) sos
       ls = map (fmtLong  ad) los
       ps = map (fmtParam ad) (names paramDescr) in
   let n = max (length ps) (max (length ls) (length ss)) in
   let ss' = ss ++ replicate (n - length ss) ""
       ls' = ls ++ replicate (n - length ls) ""
       ps' = ps ++ replicate (n - length ps) "" in
   map (\ (l, s, p) -> Opt {long = l, short = s, param = p}) (zip3 ls' ss' ps') ++ ds

fmtShort :: ArgDescr a -> Char -> String
fmtShort (NoArg  _   ) so = "-" ++ [so]
fmtShort (ReqArg _ ad) so = "-" ++ [so] ++ " " ++ ad
fmtShort (OptArg _ ad) so = "-" ++ [so] ++ "[" ++ ad ++ "]"

fmtLong :: ArgDescr a -> String -> String
fmtLong (NoArg  _   ) lo = "--" ++ lo
fmtLong (ReqArg _ ad) lo = "--" ++ lo ++ "=" ++ ad
fmtLong (OptArg _ ad) lo = "--" ++ lo ++ "[=" ++ ad ++ "]"

fmtParam :: ArgDescr a -> String -> String
fmtParam (NoArg  _   ) po = po ++ ": Yes"
fmtParam (ReqArg _ ad) po = po ++ ": " ++ ad
fmtParam (OptArg _ ad) po = po ++ ": " ++ ad
