import qualified Data.ByteString.Lazy as Buf (writeFile)
import Raster

red = RGBColor 255 0 0
black = minBound
white = maxBound

img = replicate 5 (replicate 7 white ++ black : replicate 8 white)
 ++ [replicate 5 white ++ red : red : black : replicate 8 white,
  replicate 5 white ++ red : red : black : red : red : replicate 6 white,
  replicate 7 white ++ black : red : red : replicate 6 white,
  replicate 16 black,
  replicate 4 white ++ red : red : white : black : replicate 8 white,
  replicate 4 white ++ red : red : white : black : white : red : red
   : replicate 5 white,
  replicate 7 white ++ black : white : red : red : replicate 5 white]
 ++ replicate 4 (replicate 7 white ++ black : replicate 8 white)

main = let rgb = fromList img
       in Buf.writeFile "rgbtest01.bmp" (exportBMP rgb)
        >> writeFile "rgbtest01.xpm" (exportXPM "rgbtest01" rgb)
