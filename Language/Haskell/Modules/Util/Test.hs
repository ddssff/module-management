module Language.Haskell.Modules.Util.Test
    ( repoModules
    , logicModules
    , diff
    , diff'
    , rsync
    , findHsModules
    ) where

import Data.List as List (filter, isPrefixOf, map)
import Language.Haskell.Exts.Syntax (ModuleName(..))
import Language.Haskell.Modules.Common (withCurrentDirectory)
import Language.Haskell.Modules.SourceDirs (ModKey(..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode)
import System.FilePath (joinPath, splitPath)
import System.FilePath.Find ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import System.Process (readProcess, readProcessWithExitCode)

repoModules :: [String]
repoModules =
    [ "Debian.Repo.Sync"
    , "Debian.Repo.Slice"
    , "Debian.Repo.SourcesList"
    , "Debian.Repo.PackageIndex"
    , "Debian.Repo.Types"
    , "Debian.Repo.Types.Slice"
    , "Debian.Repo.Types.Repository"
    , "Debian.Repo.Types.PackageIndex"
    , "Debian.Repo.Types.Release"
    , "Debian.Repo.Types.AptImage"
    , "Debian.Repo.Types.Repo"
    , "Debian.Repo.Types.AptBuildCache"
    , "Debian.Repo.Types.EnvPath"
    , "Debian.Repo.Types.AptCache"
    , "Debian.Repo.Orphans"
    , "Debian.Repo.Types"
    , "Debian.Repo.AptImage"
    , "Debian.Repo.Package"
    , "Debian.Repo.Monads.Top"
    , "Debian.Repo.Monads.Apt"
    , "Debian.Repo.AptCache"
    , "Tmp.File"
    , "Text.Format" ]

logicModules :: [String]
logicModules =
    [ "Data.Boolean.SatSolver"
    , "Data.Boolean"
    , "Data.Logic.Resolution"
    , "Data.Logic.KnowledgeBase"
    , "Data.Logic.Types.FirstOrder"
    , "Data.Logic.Types.Common"
    , "Data.Logic.Types.Harrison.Formulas.FirstOrder"
    , "Data.Logic.Types.Harrison.Formulas.Propositional"
    , "Data.Logic.Types.Harrison.Prop"
    , "Data.Logic.Types.Harrison.Equal"
    , "Data.Logic.Types.Harrison.FOL"
    , "Data.Logic.Types.Propositional"
    , "Data.Logic.Types.FirstOrderPublic"
    , "Data.Logic.Harrison.Unif"
    , "Data.Logic.Harrison.Meson"
    , "Data.Logic.Harrison.Herbrand"
    , "Data.Logic.Harrison.Formulas.FirstOrder"
    , "Data.Logic.Harrison.Formulas.Propositional"
    -- , "Data.Logic.Harrison.Tests"
    , "Data.Logic.Harrison.Resolution"
    , "Data.Logic.Harrison.DefCNF"
    , "Data.Logic.Harrison.Skolem"
    , "Data.Logic.Harrison.Prop"
    , "Data.Logic.Harrison.DP"
    , "Data.Logic.Harrison.Lib"
    , "Data.Logic.Harrison.PropExamples"
    , "Data.Logic.Harrison.Prolog"
    , "Data.Logic.Harrison.Tableaux"
    , "Data.Logic.Harrison.Equal"
    , "Data.Logic.Harrison.Normal"
    , "Data.Logic.Harrison.FOL"
    , "Data.Logic.Failing"
    , "Data.Logic.Instances.SatSolver"
    -- , "Data.Logic.Instances.TPTP"
    , "Data.Logic.Instances.Chiou"
    , "Data.Logic.Instances.PropLogic"
    , "Data.Logic.Normal.Clause"
    , "Data.Logic.Normal.Implicative"
    , "Data.Logic.Classes.FirstOrder"
    , "Data.Logic.Classes.Variable"
    , "Data.Logic.Classes.Apply"
    , "Data.Logic.Classes.Negate"
    , "Data.Logic.Classes.Pretty"
    , "Data.Logic.Classes.Arity"
    , "Data.Logic.Classes.Skolem"
    , "Data.Logic.Classes.Combine"
    , "Data.Logic.Classes.Constants"
    , "Data.Logic.Classes.Equals"
    , "Data.Logic.Classes.Propositional"
    , "Data.Logic.Classes.Atom"
    , "Data.Logic.Classes.Formula"
    , "Data.Logic.Classes.ClauseNormalForm"
    , "Data.Logic.Classes.Term"
    , "Data.Logic.Classes.Literal"
    , "Data.Logic.Satisfiable" ]

diff :: FilePath -> FilePath -> IO (ExitCode, String, String)
diff a b =
    do (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "--exclude=*~", "--exclude=*.imports", a, b] ""
       let out' = unlines (List.filter (not . isPrefixOf "Binary files") . List.map (takeWhile (/= '\t')) $ (lines out))
       return (code, out', err)

-- | Like diff, but ignores extra files in b.
diff' :: FilePath -> FilePath -> IO (ExitCode, String, String)
diff' a b =
    do (code, out, err) <- readProcessWithExitCode "diff" ["-ru", "--unidirectional-new-file", "--exclude=*~", "--exclude=*.imports", a, b] ""
       let out' = unlines (List.filter (not . isPrefixOf "Binary files") . List.map (takeWhile (/= '\t')) $ (lines out))
       return (code, out', err)

rsync :: FilePath -> FilePath -> IO ()
rsync a b = readProcess "rsync" ["-aHxS", "--delete", a ++ "/", b] "" >> return ()

-- | Find the paths of all the files below the directory @top@.
-- | Convenience function for building the moduVerse, searches for
-- modules in a directory hierarchy.  FIXME: This should be in
-- ModuVerse and use the value of sourceDirs to remove prefixes from
-- the module paths.  And then it should look at the module text to
-- see what the module name really is.
findHsModules :: [FilePath] -> IO [ModKey]
findHsModules tops = do
  concat <$> mapM doTop tops
    where
      doTop top = do
        files <- withCurrentDirectory top (find always (extension ==? ".hs" &&? fileType ==? RegularFile) ".")
        pure $ map (ModKey top . asModuleName . joinPath . filter (/= ".") . splitPath) files

asModuleName :: FilePath -> ModuleName ()
asModuleName path =
    -- Subtract 3 for the ".hs"
    ModuleName () (List.map (\ c -> if c == '/' then '.' else c) (take (length path - 3) path))
