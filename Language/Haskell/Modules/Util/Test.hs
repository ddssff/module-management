module Language.Haskell.Modules.Util.Test
    ( repoModules
    , logicModules
    , diff
    , diff'
    , rsync
    , findModules
    , findPaths
    ) where

import Control.Monad (foldM)
import Data.List as List (filter, isPrefixOf, isSuffixOf, map)
import Data.Set as Set (Set, empty, fromList, insert, map, unions)
import qualified Language.Haskell.Exts.Syntax as S (ModuleName(..))
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Process (readProcess, readProcessWithExitCode)

repoModules :: Set S.ModuleName
repoModules =
    Set.fromList
            [S.ModuleName "Debian.Repo.Sync",
             S.ModuleName "Debian.Repo.Slice",
             S.ModuleName "Debian.Repo.SourcesList",
             S.ModuleName "Debian.Repo.PackageIndex",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.Types.Slice",
             S.ModuleName "Debian.Repo.Types.Repository",
             S.ModuleName "Debian.Repo.Types.PackageIndex",
             S.ModuleName "Debian.Repo.Types.Release",
             S.ModuleName "Debian.Repo.Types.AptImage",
             S.ModuleName "Debian.Repo.Types.Repo",
             S.ModuleName "Debian.Repo.Types.AptBuildCache",
             S.ModuleName "Debian.Repo.Types.EnvPath",
             S.ModuleName "Debian.Repo.Types.AptCache",
             S.ModuleName "Debian.Repo.Orphans",
             S.ModuleName "Debian.Repo.Types",
             S.ModuleName "Debian.Repo.AptImage",
             S.ModuleName "Debian.Repo.Package",
             S.ModuleName "Debian.Repo.Monads.Top",
             S.ModuleName "Debian.Repo.Monads.Apt",
             S.ModuleName "Debian.Repo.AptCache",
             S.ModuleName "Tmp.File",
             S.ModuleName "Text.Format"]

logicModules :: Set S.ModuleName
logicModules =
    Set.fromList
       [ S.ModuleName "Data.Boolean.SatSolver"
       , S.ModuleName "Data.Boolean"
       , S.ModuleName "Data.Logic.Resolution"
       , S.ModuleName "Data.Logic.KnowledgeBase"
       , S.ModuleName "Data.Logic.Types.FirstOrder"
       , S.ModuleName "Data.Logic.Types.Common"
       , S.ModuleName "Data.Logic.Types.Harrison.Formulas.FirstOrder"
       , S.ModuleName "Data.Logic.Types.Harrison.Formulas.Propositional"
       , S.ModuleName "Data.Logic.Types.Harrison.Prop"
       , S.ModuleName "Data.Logic.Types.Harrison.Equal"
       , S.ModuleName "Data.Logic.Types.Harrison.FOL"
       , S.ModuleName "Data.Logic.Types.Propositional"
       , S.ModuleName "Data.Logic.Types.FirstOrderPublic"
       , S.ModuleName "Data.Logic.Harrison.Unif"
       , S.ModuleName "Data.Logic.Harrison.Meson"
       , S.ModuleName "Data.Logic.Harrison.Herbrand"
       , S.ModuleName "Data.Logic.Harrison.Formulas.FirstOrder"
       , S.ModuleName "Data.Logic.Harrison.Formulas.Propositional"
       -- , S.ModuleName "Data.Logic.Harrison.Tests"
       , S.ModuleName "Data.Logic.Harrison.Resolution"
       , S.ModuleName "Data.Logic.Harrison.DefCNF"
       , S.ModuleName "Data.Logic.Harrison.Skolem"
       , S.ModuleName "Data.Logic.Harrison.Prop"
       , S.ModuleName "Data.Logic.Harrison.DP"
       , S.ModuleName "Data.Logic.Harrison.Lib"
       , S.ModuleName "Data.Logic.Harrison.PropExamples"
       , S.ModuleName "Data.Logic.Harrison.Prolog"
       , S.ModuleName "Data.Logic.Harrison.Tableaux"
       , S.ModuleName "Data.Logic.Harrison.Equal"
       , S.ModuleName "Data.Logic.Harrison.Normal"
       , S.ModuleName "Data.Logic.Harrison.FOL"
       , S.ModuleName "Data.Logic.Failing"
       , S.ModuleName "Data.Logic.Instances.SatSolver"
       -- , S.ModuleName "Data.Logic.Instances.TPTP"
       , S.ModuleName "Data.Logic.Instances.Chiou"
       , S.ModuleName "Data.Logic.Instances.PropLogic"
       , S.ModuleName "Data.Logic.Normal.Clause"
       , S.ModuleName "Data.Logic.Normal.Implicative"
       , S.ModuleName "Data.Logic.Classes.FirstOrder"
       , S.ModuleName "Data.Logic.Classes.Variable"
       , S.ModuleName "Data.Logic.Classes.Apply"
       , S.ModuleName "Data.Logic.Classes.Negate"
       , S.ModuleName "Data.Logic.Classes.Pretty"
       , S.ModuleName "Data.Logic.Classes.Arity"
       , S.ModuleName "Data.Logic.Classes.Skolem"
       , S.ModuleName "Data.Logic.Classes.Combine"
       , S.ModuleName "Data.Logic.Classes.Constants"
       , S.ModuleName "Data.Logic.Classes.Equals"
       , S.ModuleName "Data.Logic.Classes.Propositional"
       , S.ModuleName "Data.Logic.Classes.Atom"
       , S.ModuleName "Data.Logic.Classes.Formula"
       , S.ModuleName "Data.Logic.Classes.ClauseNormalForm"
       , S.ModuleName "Data.Logic.Classes.Term"
       , S.ModuleName "Data.Logic.Classes.Literal"
       , S.ModuleName "Data.Logic.Satisfiable" ]

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
findPaths :: [FilePath] -> IO (Set FilePath)
findPaths tops =
    foldM doPath empty tops
    where
      doPath r path =
          do dir <- doesDirectoryExist path
             reg <- doesFileExist path
             case () of
               _ | dir -> doDirectory r path
               _ | reg && isSuffixOf ".hs" path -> return (insert path r)
               _ -> return r
      doDirectory r path =
          getDirectoryContents path >>= foldM doPath r . List.map (path </>) . filter (\ x -> x /= "." && x /= "..")

-- | Convenience function for building the moduVerse, searches for
-- modules in a directory hierarchy.  FIXME: This should be in
-- MonadClean and use the value of sourceDirs to remove prefixes from
-- the module paths.  And then it should look at the module text to
-- see what the module name really is.
findModules :: [FilePath] -> IO (Set S.ModuleName)
findModules tops =
    findPaths tops >>= return . Set.map asModuleName
    where
      asModuleName path =
          S.ModuleName (List.map (\ c -> if c == '/' then '.' else c) (take (length path - 3) path))
