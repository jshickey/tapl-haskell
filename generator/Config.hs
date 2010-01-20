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

useType :: String -> Config -> Bool
useType t (Config _ (Types ts) _ _) = t `elem` ts

hasOption :: String -> Config -> Bool
hasOption t (Config _ _ _ (Options ts)) = t `elem` ts

hasSubtypes = hasOption "subtypes"
useIsorec = useType "isorec"
useEquirec = useType "equirec"

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