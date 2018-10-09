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
-- import System.Process (createProcess, waitForProcess, shell, StdStream, std_in)
import System.Process
import System.Exit (ExitCode(..))


main = do
    args <- getArgs
    getCurrentDirectory >>= findRepos >>= runCommands args

findRepos :: FilePath -> IO [FilePath]
findRepos path =
    listDirectory path
    >>= appendPath path
    >>= excludeBadPaths
    >>= filterM isGit

appendPath :: FilePath -> [FilePath] -> IO [FilePath]
appendPath path paths = return $ map ((path ++"/") ++) paths

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

isGit :: FilePath -> IO Bool
isGit path = do
    paths <- listDirectory path
    if ".git" `elem` paths
       then return $ True
       else return $ False

runGitCommand :: (String, FilePath) -> IO ()
runGitCommand (command, gitRepo) = do
    -- (_, Just out, _, ph) <- createProcess (shell $ cmd gitRepo) { std_out = CreatePipe}
    (_, out, _, ph) <- createProcess $ shell $ cmd gitRepo
    exit <- waitForProcess ph
    case exit of
        -- ExitSuccess -> hGetContents out >>= print
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
