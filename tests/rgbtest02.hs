import qualified Data.ByteString.Lazy as Buf (writeFile)
import Raster

main = Buf.writeFile "rgbtest02.bmp" $ exportBMP $ fromPixels (17, 17)
 [((y', x'), RGBColor x y 0) | x' <- [0..16], y' <- [0..16],
  let x = fromIntegral (if x' == 16 then 255 else x' * 16),
  let y = fromIntegral (if y' == 16 then 255 else y' * 16)]
