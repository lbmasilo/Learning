module Main where

import DataStructures
import InputOutput
import Processing

import System.Environment
import Data.List

main :: IO ()
main = do
  lstArgs <- getArgs 
  let strInputFileName = head lstArgs
  let strOutputFileName = lstArgs !! 1
  record <- readDataFromFile strInputFileName
  let imageSize = dimensions record
  let lstCircles = circles record
  putStrLn "Generating canvas"
  let imgCanvas = createImage imageSize (255, 255, 255)
  putStrLn "processing circles"
  let lstBlendedImages = blendCircleWithImage imgCanvas lstCircles
  let strPPM = convertToPPM $ head lstBlendedImages
  writeDataToFile strPPM strOutputFileName
  putStrLn "PPM file generated"