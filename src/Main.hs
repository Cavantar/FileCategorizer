{-# LANGUAGE TemplateHaskell, DeriveGeneric, StandaloneDeriving #-}

module Main where

import Data.Aeson (FromJSON, parseJSON, genericParseJSON, defaultOptions, eitherDecode)
import Data.Aeson.Types (fieldLabelModifier)
import Data.List (isPrefixOf, nub)
import Data.Maybe (isJust)
import Data.Function ((&))
import System.IO (hSetEncoding, stdout, utf8)
import System.Win32.Console (setConsoleOutputCP)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, takeExtension, (</>))
import System.FilePath.Find (FileType(..), fileType, find, always, (/=?))
import System.Directory (renameFile, doesFileExist, createDirectoryIfMissing, removeDirectoryRecursive)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Safe (headMay)
import Text.Regex
import GHC.Generics

import qualified Data.ByteString.Lazy as BSL

data DirCategory       = DirCategory {_dirName :: String, _dirRegex :: [String]} deriving (Show, Generic)
data CategorizerConfig = CatConf     {_dirCategories :: [DirCategory], _uncategorizedDir :: String}
                       deriving (Show, Generic)

instance FromJSON DirCategory       where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }
instance FromJSON CategorizerConfig where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

try' :: IO a -> (SomeException -> IO b) -> (a -> IO b) -> IO b
try' comp left right = try comp >>= either left right

configFileName :: String
configFileName = "fileCategorizerConfig.json"

isMatched :: DirCategory -> String -> Bool
isMatched (DirCategory _ dirRegex) = or.map isJust . flip map (map (matchRegex.mkRegex) dirRegex) . (&)

matchDir :: String -> String -> [DirCategory] -> String
matchDir uncategorizedDir filename =  maybe uncategorizedDir _dirName .headMay.filter (flip isMatched (takeFileName filename))

getClosestNameIndexed :: FilePath -> Int -> IO FilePath
getClosestNameIndexed filePath index = do
  let newFilePath = (takeDirectory filePath) </> (takeFileName filePath ++ "_" ++ (show index) ++ (takeExtension filePath))
  exist <- doesFileExist newFilePath
  if not exist then return newFilePath
    else getClosestNameIndexed filePath (index+1)

getClosestName :: FilePath -> IO FilePath
getClosestName filePath = do
  createDirectoryIfMissing True (takeDirectory filePath)
  exist <- doesFileExist filePath
  if not exist then return filePath
    else getClosestNameIndexed filePath 0

moveFileSafe :: (FilePath, FilePath) -> IO ()
moveFileSafe (src, dst) = getClosestName dst >>= (\newDst -> try' (renameFile src newDst) (print) (return))

shouldBeExcluded :: [FilePath] -> FilePath -> Bool
shouldBeExcluded excludedDirs filePath = or $ map (flip isPrefixOf filePath) excludedDirs

main :: IO ()
main = do
  hSetEncoding stdout utf8
  setConsoleOutputCP 65001
  try' (BSL.readFile configFileName) (\exc -> putStrLn $ "Couldn't open config file: " ++ (show exc)) $ either
    (\err -> putStrLn $ "Error parsing config file: " ++ err)
    (\(CatConf dirCategories uncategorizedDir) -> do
         headMay <$> getArgs >>= maybe (putStrLn "No dirPath specified !")
           (\startDir -> do
                files <- map (drop (length startDir + 1)) <$> find always (fileType /=? Directory) startDir
                let excludedDirs = uncategorizedDir : (map _dirName dirCategories)
                    removedExcludedDirs = filter (not . shouldBeExcluded excludedDirs) files
                    fileMatches         =
                      map (\filepath -> (startDir </> filepath,
                                         startDir </> (matchDir uncategorizedDir filepath dirCategories) </> (takeFileName filepath)))
                      removedExcludedDirs
                forM_ fileMatches (\(src, dst) -> do
                                    putStrLn $ src ++ " ----------------> " ++ dst
                                    moveFileSafe (src, dst)
                                    )
                putStrLn "\nRemoving Directories--------------------\n"
                let directoriesToRemove  = map (startDir </>) $ filter (not . shouldBeExcluded excludedDirs) $
                                           nub $ map (takeWhile (/='\\')) $ filter (/= ".") $ nub $ map takeDirectory files
                forM_ directoriesToRemove (\directory -> try' (removeDirectoryRecursive directory) (print) (return))
           )
    ).eitherDecode
