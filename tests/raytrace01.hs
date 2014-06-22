import qualified Data.ByteString.Lazy as Buf (writeFile)
import Math.Vector
import Raster (fnewRaster, exportBMP, RGBColor(..))
import Raster.Color (toRGB)
import Raster.Palette
import Raytrace

width, height :: Num a => a
--width = 450; height = 364
width  = 200
height = 200

main = Buf.writeFile "raytrace01.bmp" $ exportBMP $ fnewRaster
 (toRGB . traceRay scene 6 . (,) camera . (<-> camera) . pixToCoord)
 (height, width)

{-
camera :: Vec3 Double
camera = Vec3 (-15) 0 0

(zu, zb) = (20, -20)
(yl, yr) = (20, -20)

pixToCoord (y, x) = Vec3 3 (yl - fromIntegral x * (yl-yr)/width) (zu - fromIntegral y * (zu-zb)/height)

scene = [
 (plane (Vec3 0 0 1) (-10), Material white 0.5 0.5 0 Nothing),
 (sphere (Vec3 12 (-10) 10) 7, Material green 0.7 0 0 Nothing),
 (sphere (Vec3 20 15 10) 5, Material grey 1 0 0 Nothing),
 mkLight (Vec3 25 0 20) 3 blue,
 mkLight (Vec3 (-20) 0 0) 3 white]
-}

camera :: Vec3 Double
camera = Vec3 0 0 (-5)

pixToCoord (y, x) = Vec3 (-4 + fromIntegral x * 8/width) (3 - fromIntegral y * 6/height) 0

-- Note: The default diffusion in Bikker's code is 0.2.
scene = [
 (plane (Vec3 0 1 0) (-4.4), Material (rtcolor 0.4 0.3 0.3) 1 0 0 Nothing),
 (sphere (Vec3 1 (-0.8) 3) 2.5, Material (rtcolor 0.7 0.7 0.7) 0.2 0.8 0.6 Nothing),
 (sphere (Vec3 (-5.5) (-0.5) 7) 2, Material (rtcolor 0.7 0.7 1) 0.1 0.9 1 Nothing),
 mkLight (Vec3 0 5 5) 0.1 (rtcolor {-0.4 0.4 0.4-} 0.6 0.6 0.6),
 mkLight (Vec3 2 5 1) 0.1 (rtcolor {-0.6 0.6 0.8-} 0.7 0.7 0.9)]
