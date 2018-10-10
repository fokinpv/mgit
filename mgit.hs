import Control.Monad (filterM, liftM, mapM)
import Data.List (intercalate)
import System.Directory
    ( listDirectory
    , getCurrentDirectory
    , makeAbsolute
    , getPermissions
    , readable
    )
import System.Environment (getArgs)
import System.IO
import System.Posix.Files (getFileStatus, isDirectory, isSymbolicLink)
import System.Process (createProcess, waitForProcess, shell)
import System.Exit (ExitCode(..))

runGitCommand :: (String, FilePath) -> IO ()
runGitCommand (command, gitRepo) = do
    (_, out, _, ph) <- createProcess $ shell $ cmd gitRepo
    exit <- waitForProcess ph
    case exit of
        ExitSuccess -> do
            case out of
                Just hout -> hGetContents hout >>= print
                Nothing -> return ()
        ExitFailure code -> print code
    where
        cmd :: FilePath -> String
        cmd repo = unwords [
                "git", "-c", "color.ui=always", "-C", repo, command
            ]

runCommands :: [String] -> [FilePath] -> IO ()
runCommands args repos = sequence_ $ fmap runGitCommand cmdrepoPairs
    where cmdrepoPairs = [(unwords args, repo) | repo <- repos]

excludeBadPaths :: [FilePath] -> IO [FilePath]
excludeBadPaths paths = filterM isValid paths
    where
        isValid :: FilePath -> IO Bool
        isValid path = do
            status <- getFileStatus path
            permissions <- getPermissions path
            return $
                isDirectory status
                && not (isSymbolicLink status)
                && readable permissions

appendPath :: FilePath -> [FilePath] -> IO [FilePath]
appendPath path paths = return $ map ((path ++"/") ++) paths

isGit :: FilePath -> IO Bool
isGit path = do
    paths <- listDirectory path
    if ".git" `elem` paths
       then return $ True
       else return $ False

findRepos :: FilePath -> IO [FilePath]
findRepos path =
    listDirectory path
    >>= appendPath path
    >>= excludeBadPaths
    >>= filterM isGit

main = do
    args <- getArgs
    getCurrentDirectory >>= findRepos >>= runCommands args
