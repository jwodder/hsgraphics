import Raster
import ReadXBM2
main = getContents >>= mapM_ (\((name, hot, img), trail) ->
 putStrLn name >> print hot >> mapM_ (\y -> putStrLn [if getPixel img (y, x)
  then '#' else '.' | x <- [0..width img - 1]]) [0..height img - 1]
 >> print trail) . readXBM
