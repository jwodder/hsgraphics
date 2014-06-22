import Raster

sqsz = 16

main = writeFile "test01.xbm" $ exportXBM "test01" Nothing
 $ fromRaggedPixels (8*sqsz, 8*sqsz) False [((y*sqsz + a, x*sqsz + b), True)
  | y <- [0..7], x <- (if even y then [1,3..7] else [0,2..7]),
    a <- [0..sqsz-1], b <- [0..sqsz-1]]
