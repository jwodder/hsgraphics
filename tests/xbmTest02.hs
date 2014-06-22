import Raster

sqsz = 16

main = writeFile "test02.xbm" $ exportXBM "test02" Nothing
 $ fnewRaster (\(y, x) -> toEnum $ (y `div` sqsz + x `div` sqsz) `mod` 2)
  (8 * sqsz, 8 * sqsz)
