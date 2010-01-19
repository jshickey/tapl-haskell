module Config where

import System.IO
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, createDirectory)
import Control.Monad.Error
import TaplError
    
newtype Terms = Terms [String]

newtype Types = Types [String]

newtype Tests = Tests [String]

newtype Options = Options [String]
    
data Config = Config {terms::Terms,
                      types::Types,
                      tests::Tests,
                      options::Options}

hasType :: Config -> String -> Bool
hasType (Config _ (Types ts) _ _) t = t `elem` ts

hasOption :: Config -> String -> Bool
hasOption (Config _ _ _ (Options ts)) t = t `elem` ts

type IOThrowsError = ErrorT TaplError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

path = "gen"

createPath :: FilePath -> IO ()
createPath path = do exists <- doesDirectoryExist path
                     if exists
                       then return ()
                       else createDirectory path
                         
writeToFile :: String -> String -> IO ()
writeToFile filename contents =
    do createPath path
       writeFile (path </> filename) contents