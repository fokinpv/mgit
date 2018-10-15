import Control.Monad (filterM, liftM, liftM2, mapM, forM, foldM)
import Data.List (partition)
import Data.Monoid ((<>))
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
        ExitFailure code -> return ()
    where
        cmd :: FilePath -> String
        cmd repo = unwords [
                "git", "-c", "core.pager=''", "-C", repo, command
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

isNotGit :: FilePath -> IO Bool
isNotGit path = fmap not (isGit path)

findRepos :: FilePath -> IO [FilePath]
findRepos path =
    listDirectory path
    >>= appendPath path
    >>= excludeBadPaths
    >>= filterM isGit

findRepos' :: FilePath -> IO [FilePath]
findRepos' path = do
    print $ "path= " <> path
    paths <- listDirectory path >>= appendPath path >>= excludeBadPaths
    putStr "paths= "
    print paths
    gitPaths <- filterM isGit paths
    a <- filterM isNotGit paths >>= mapM findRepos'
    putStr "a= "
    print a
    print $ null a
    return $ gitPaths <> liftM (foldl (<>) []) a
    -- if paths == []
    --    then return []
    --    else do
    --         gitPaths <- filterM isGit paths
    --         a <- filterM isNotGit paths >>= mapM findRepos'
    --         putStr "a= "
    --         print a
    --         return $ gitPaths <> liftM (foldl (<>) []) a

main = do
    args <- getArgs
    -- getCurrentDirectory >>= findRepos' >>= print
    getCurrentDirectory >>= findRepos' >>= print . length
    -- if "count" `elem` args
    --    then getCurrentDirectory >>= findRepos >>= print . length
    --    else getCurrentDirectory >>= findRepos >>= runCommands args
