import qualified Data.ByteString.Lazy as Buf (writeFile)
import Raster
import Ternary

main = Buf.writeFile "rgbtest02.bmp" $ exportBMP $ fromPixels (17, 17)
 [((y', x'), RGBColor x y 0) | x' <- [0..16], y' <- [0..16],
  let x = fromIntegral $ x' == 16 ?: 255 :? x' * 16,
  let y = fromIntegral $ y' == 16 ?: 255 :? y' * 16]
