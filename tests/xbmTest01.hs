import Raster
import Ternary

sqsz = 16

main = writeFile "test01.xbm" $ exportXBM "test01" Nothing
 $ fromRaggedPixels (8*sqsz, 8*sqsz) False [((y*sqsz + a, x*sqsz + b), True)
  | y <- [0..7], x <- (even y ?: [1,3..7] :? [0,2..7]),
    a <- [0..sqsz-1], b <- [0..sqsz-1]]
