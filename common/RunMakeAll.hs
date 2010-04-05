{- Helper script to apply a make operation to all subdirectories.

   To see usage, execute: run-make-all --help
  
   Example usage: run-make-all --command=test --dir=../
     Run from the "common" subdirectory, this example command will
     run the unit tests in all directories, and will exit with an error
     as soon as it encounters any test error or failure.
 -}
module Main( main ) where

import System( getArgs )
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import System.Directory
import System.FilePath
import System.Cmd
import System.Exit
import Control.Monad

-- TODO: make this empty
inProgress = ["fullequirec", "fullisorec", "equirec"]

main :: IO ()
main = do args <- getArgs
          case getOpt Permute options args of
            (flags,[],[]) -> runFlags flags
            (_,_,errors@(_:_)) -> error $ show errors ++ "\n" ++ usage
            (_,ignored@(_:_),_) -> error $ "Ignored: " ++ show ignored ++ "\n" ++ usage

runFlags :: [Flag] -> IO ()
runFlags flags = if hasHelp flags 
                   then putStrLn usage
                   else (getCmd flags) (getDir flags)

hasHelp :: [Flag] -> Bool
hasHelp []       = False
hasHelp (Help:_) = True
hasHelp (_:hs)   = hasHelp hs

getDir :: [Flag] -> String
getDir ((Dir d):_) = d
getDir (_:fs) = getDir fs
          
getCmd :: [Flag] -> (String -> IO ())
getCmd ((Cmd c):_) | c == "make"  = execInDirs ""
                   | otherwise    = execInDirs c
getCmd (_:cs) = getCmd cs 

execInDirs :: String -> String -> IO ()
execInDirs cmd dir
    = do dirContents <- liftM (map (dir </>)) (getDirectoryContents dir)
         subDirs <- filterM doesDirectoryExist dirContents
         let validSubDirs = filter (valid . snd . splitFileName) subDirs
         execute validSubDirs cmd
    where valid dir = (head dir) /= '.' && (dir /= "gen") &&
                      (not (dir `elem` inProgress))

execute [] cmd = putStrLn "\nAll make commands completed successfully." >>
                 if cmd == "test"
                   then putStrLn "For the '--command=test' option, this implies that there were no errors or failures in any of the test suites."
                   else return ()
execute (d:ds) cmd = do exitCode <- system $ "make --directory=" ++ d ++ " " ++ cmd
                        case exitCode of
                          ExitSuccess -> execute ds cmd
                          ExitFailure v -> error "Make command failed"

data Flag = Help
          | Dir String
          | Cmd String
            deriving Show

dirp,cmdp :: Maybe String -> Flag
dirp = Dir . fromMaybe "."
cmdp = Cmd . fromMaybe "make"

usage = usageInfo "Usage: run-make-all [OPTION...]" options

options = [ Option ['c'] ["command"] (OptArg cmdp "clean|test|make") "the make command to run.  The default is 'make', which simply executes a straight-up make in each directory"
          , Option ['d'] ["dir"] (OptArg dirp "DIR") "root directory (i.e., one level above common).  The default is the current directory."
          , Option ['h'] ["help"] (NoArg Help) "show usage"
          ]
