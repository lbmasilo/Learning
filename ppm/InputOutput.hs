module InputOutput
(
  writeDataToFile,
  readDataFromFile
)where

import System.IO
import DataStructures

writeDataToFile :: String -> String -> IO ()
writeDataToFile strContents strFilename = do
  hndFile <- openFile strFilename WriteMode
  hPutStr hndFile strContents
  hClose hndFile

readDataFromFile strFilename = do
  hndFile <- openFile strFilename ReadMode
  strContents <- hGetContents hndFile
  let record = read strContents :: CircleSet
  putStr "Read from file"
  putStrLn strContents
  hClose hndFile
  return record;