module Config where

import System.IO
import System.FilePath ((</>), splitPath, joinPath)
import System.Directory (doesDirectoryExist, createDirectory,
                         getCurrentDirectory, copyFile)
import Control.Monad.Error
import TaplError
    
newtype Terms = Terms [String]

newtype Types = Types [String]

newtype Tests = Tests [String]

newtype Options = Options [String]
    
data Config = CopyConfig { name::String
                         , files::[String]
                         , notes::String}
            | GenConfig { name::String
                        , terms::Terms
                        , types::Types
                        , tests::Tests
                        , options::Options
                        , notes::String}

useType :: String -> Config -> Bool
useType t (GenConfig _ _ (Types ts) _ _ _) = t `elem` ts

hasOption :: String -> Config -> Bool
hasOption t (GenConfig _ _ _ _ (Options ts) _) = t `elem` ts

hasSubtypes = hasOption "subtypes"
useIsorec = useType "isorec"
useEquirec = useType "equirec"

type IOThrowsError = ErrorT TaplError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

genDest :: FilePath -> IO FilePath
genDest f = do d <- getCurrentDirectory
               let ds = splitPath d
               let baseGenPath = (joinPath (init ds)) </> "gen"
               createPath baseGenPath
               let output = baseGenPath </> (last ds)
               createPath output
               return $ output </> (last (splitPath f))

createPath :: FilePath -> IO ()
createPath path = do exists <- doesDirectoryExist path
                     if exists
                       then return ()
                       else createDirectory path
                         
writeToFile :: FilePath -> String -> IO ()
writeToFile filename contents = do dest <- genDest filename
                                   writeFile dest contents

copyToGen :: FilePath -> IO ()
copyToGen filename = do dest <- genDest filename
                        copyFile filename dest
