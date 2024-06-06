module InputOutput
(
  readDataFromFile,
  writeDataSetTofile
) where

import System.IO
import DataStructure

readDataFromFile :: String -> IO DataSet
readDataFromFile strFileName = do
    hndFile <- openFile strFileName ReadMode
    strDataSet <- hGetContents hndFile
    let record = read strDataSet :: DataSet
    putStrLn $ "Loaded: " ++ strDataSet
    hClose hndFile
    return record

writeDataToFile :: String -> String -> IO ()
writeDataToFile strFileName strContents = do
    hndFile <- openFile strFileName WriteMode
    hPutStrLn hndFile strContents
    hClose hndFile

writeDataSetTofile :: String -> DataSet -> IO ()
writeDataSetTofile strFileName recDataset = writeDataToFile strFileName strDataSet
   where strDataSet = show recDataset
