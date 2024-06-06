module Processing
(
  convertToPPM,
  blendCircleWithImage,
  createImage
) where

import DataStructures

calcDistance :: Position -> Position -> Double
calcDistance p q = sqrt(s)
  where 
  s = fromIntegral $ a + b
  a = (p1 - q1) ^ 2
  b = (p2 - q2) ^ 2
  p1 = fst p
  q1 = fst q
  p2 = snd p
  q2 = snd q

isInRadius :: Pixel -> Circle -> Bool
isInRadius recPixel recCircle = calcDistance pPos cPos <= radius
  where 
    pPos = position recPixel
    (cPos, radius, _) = recCircle

createPixels :: Dimensions -> Colour -> [Pixel]
createPixels (rows, cols) col = 
  let
   rs = [1..rows]
   cs = [1..cols]
   ps = [(r,c) | r <- rs, c <- cs]
  in [Pixel {position = p, colour = col} | p <- ps]

createImage :: Dimensions -> Colour -> Image
createImage dim col = Image {size = dim, pixels = createPixels dim col}

colourToString :: Colour -> String
colourToString (red, green, blue) = show red ++ " " ++ show green ++ " " ++ show blue ++ " "

toPixelString :: [Pixel] -> String
toPixelString [] = ""
toPixelString (p:ps) = colourToString (colour p) ++ " " ++ toPixelString ps

convertToPPM :: Image -> String
convertToPPM imgCanvas = strHead ++ strPixels
  where 
   strHead = "P3\n" ++ strDimensions ++ "\n255\n"
   strCols = show $ snd $ dims
   strRows = show $ fst $ dims
   dims = size imgCanvas
   strDimensions = strCols ++ " " ++ strRows
   strPixels = toPixelString $ pixels imgCanvas

blendColour :: Colour -> Colour -> Colour
blendColour c1 c2 = (blend c1Red c2Red, blend c1Green c2Green, blend c1Blue c2Blue)
  where (c1Red, c1Green, c1Blue) = c1
        (c2Red, c2Green, c2Blue) = c2
        blend a b = let 
                        v1 = fromIntegral a
                        v2 = fromIntegral b
                    in round((v1 + v2) /2)

blendPixels :: [Pixel] -> Circle -> [Pixel]
blendPixels [] _ = []
blendPixels (p:ps) recCircle
  | isInRadius p recCircle = Pixel {position = posP, colour = blendColour colP colC} : blendPixels ps recCircle
  | otherwise = p : blendPixels ps recCircle
  where 
   posP = position p
   colP = colour p
   (_, _, colC) = recCircle

addCircle :: Image -> Circle -> Image
addCircle imgCanvas recCircle = Image {size = s, pixels = blendPixels ps recCircle}
  where 
   s = size imgCanvas
   ps = pixels imgCanvas

blendCircleWithImage :: Image -> Circles -> [Image]
blendCircleWithImage imgCanvas [] = []
blendCircleWithImage imgCanvas (c:cs) = imgNew : blendCircleWithImage imgNew cs
  where imgNew = addCircle imgCanvas c 

