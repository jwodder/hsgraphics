{- TODO:
 * Add functions for ANDing, ORing, and XORing two Bitmaps together
 * Add resizing and/or scaling functions?
 * Add a function for combining two or more Rasters into a single image?
 * Add a function for invoking 'ixmap' on a Raster's internal array
 * Possibly add map functions with the type signatures:
  * (Coord -> a -> Coord) -> Raster a -> Raster a
  * (Coord -> a -> (Coord, b)) -> Raster a -> Raster b
 * Add functions for importing & exporting NetPBM images
 * Switch the names of the {to,from}Pixels and {to,from}List functions?
 * Add a readXPM function that constructs a Raster of a custom tagged union of
   RGB values, HSV values, symbolic names, and "None"
  * Add an advanced alternative to exportXPM that uses the above datatype
 * Add a Bitmap constructor that takes a [Coord]?
-}

module Raster (
  -- * Types
  Raster, Coord, RGBColor(..), Bitmap, Greymap, RGBImage,
  -- * Construction
  newRaster, fnewRaster,
  fromList, fromRaggedList, fromFlatList,
  fromPixels, fromRaggedPixels,
  fromArray,
  -- * Deconstruction
  toList, toFlatList, toPixels, toArray,
  -- * Querying
  size, width, height, inRaster, getPixel, getSubRaster, palette,
  -- * Modification
  setPixel, setPixels, mapWithCoords,
  -- * Exporting
  exportBMP, exportXPM, exportXBM,
  -- * Other
  extractMaybes
 ) where
 import Data.Array
 import Data.Word (Word8)
 import Numeric (showHex)
 import Data.Binary.Put
 import Data.ByteString.Lazy (ByteString)
 import qualified Data.Map as Map
 import Raster.Color

 -- |A (y, x) coordinate pair with (0, 0) being the upper-left corner of the
 -- image and the y-axis increasing downwards
 type Coord = (Int, Int)

 newtype Raster a = Raster (Array Coord a) deriving (Eq, Ord, Read, Show)

 instance Functor Raster where fmap f (Raster dat) = Raster (fmap f dat)

 type Bitmap   = Raster Bool
 type Greymap  = Raster Word8
 type RGBImage = Raster RGBColor

 toList :: Raster a -> [[a]]  -- list of rows from top to bottom
 toList (Raster dat) = clump $ elems dat
  where clump [] = []
	clump xs = let (h, t) = splitAt w xs in h : clump t
	w = succ $ snd $ snd $ bounds dat

 getPixel :: Raster a -> Coord -> a
 getPixel (Raster dat) = (dat !)

 setPixel :: Raster a -> Coord -> a -> Raster a
 setPixel (Raster dat) xy pix = Raster $ dat // [(xy, pix)]

 -- |Returns the height and width of a 'Raster', in that order
 size :: Raster a -> (Int, Int)
 size (Raster dat) = (h+1, w+1) where (h, w) = snd (bounds dat)

 width, height :: Raster a -> Int
 width = snd . size
 height = fst . size

 -- |Tests whether the given coordinate is inside the 'Raster'
 inRaster :: Raster a -> Coord -> Bool
 inRaster (Raster dat) = inRange (bounds dat)

 -- |@newRaster (height, width) fill@ constructs a 'Raster' of height @height@
 -- and width @width@ in which every pixel is set to @fill@.
 newRaster :: (Int, Int) -> a -> Raster a
 newRaster (h, w) fill = Raster $ listArray ((0, 0), (h-1, w-1)) $ repeat fill

 -- |Returns a 'Map' from all colors used in an image to their frequencies
 palette :: Ord a => Raster a -> Map.Map a Int
 palette (Raster dat) = Map.fromListWith (+) $ zip (elems dat) (repeat 1)

 toArray :: Raster a -> Array Coord a
 toArray (Raster dat) = dat

 fromPixels :: (Int, Int) -> [(Coord, a)] -> Raster a
  -- requires that all pixels be present in the list exactly once
 fromPixels (h, w) = Raster . array ((0, 0), (h-1, w-1))

 -- |@fromList@ converts a (non-empty) list of rows of pixels into a 'Raster'.
 -- Each sublist must be exactly the same length.  The list is interpreted as
 -- storing the pixels of an image from left to right and then top to bottom
 -- (i.e., in the same format returned by 'toList').
 fromList :: [[a]] -> Raster a
 fromList pix = Raster $ listArray ((0, 0), (length pix - 1,
  length (head pix) - 1)) $ concat pix

 fromRaggedList :: (Int, Int) -> a -> [[a]] -> Raster a
 fromRaggedList (h, w) fill pix = Raster $
  accumArray (\_ x -> x) fill ((0, 0), (h-1, w-1))
   [((y, x), c) | (y, row) <- zip [0..h-1] pix, (x, c) <- zip [0..w-1] row]

 fromRaggedPixels :: (Int, Int) -> a -> [(Coord, a)] -> Raster a
 fromRaggedPixels (h, w) fill = Raster . accumArray (\_ x -> x) fill
  ((0, 0), (h-1, w-1)) . filter (inRange ((0, 0), (h-1, w-1)) . fst)

 fromArray :: Array Coord a -> Raster a
 fromArray dat = Raster $ ixmap ((0, 0), (y2-y1, x2-x1))
  (\(b, a) -> (b + y1, a + x1)) dat
  where ((y1, x1), (y2, x2)) = bounds dat

 exportBMP :: RGBImage -> ByteString
 exportBMP rgb@(Raster dat) =
  let (h, w) = size rgb
      (qty, colors) = Map.mapAccum (\i _ -> (i+1, i)) 0 (palette rgb)
      bits = if qty <= 2 then 1
	     else if qty <= 16 then 4
	     else if qty <= 256 then 8
	     else 24
      rs' = (w * bits `div` 8) + (fromEnum $ mod (w * bits) 8 /= 0)
      rowSize = if mod rs' 4 == 0 then rs' else rs' + 4 - rs' `mod` 4
      nonDat = 54 + (if bits == 24 then 0 else qty * 4)
      pword  = putWord16le . fromIntegral  -- pack/put word
      pdword = putWord32le . fromIntegral  -- pack/put dword
      bits2bytes [] = []
      bits2bytes bs = foldl (\x b -> x * 2 ^ bits + b) 0
		       (take (8 `div` bits) (bs ++ repeat 0))
		       : bits2bytes (drop (8 `div` bits) bs)
  in runPut $ putWord8 0x42 >> putWord8 0x4D >> pdword (nonDat + h * rowSize)
   >> pdword 0 >> pdword nonDat >> pdword 0x28 >> pdword w >> pdword h
   >> pword 1 >> pword bits >> pdword 0 >> pdword (h * rowSize) >> pdword 0
   >> pdword 0 >> pdword qty >> pdword 0
   >> if bits == 24 then
    mapM_ (\y -> mapM_ putWord8 $ take rowSize $ concat
     [[b, g, r] | x <- [0..w-1], let (RGBColor r g b) = dat ! (y, x)]
     ++ repeat 0) [h-1, h-2 .. 0]
    else mapM_ (pdword . fromEnum) (Map.keys colors) >> mapM_ (\y ->
     mapM_ (putWord8 . fromIntegral) $ take rowSize $ bits2bytes
     [colors Map.! (dat ! (y, x)) | x <- [0..w-1]] ++ repeat 0) [h-1, h-2 .. 0]

 xpmChars :: String
 xpmChars = "!#$%&'()*+,-./0123456789:;<=>@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`\
  \abcdefghijklmnopqrstuvwxyz{|}~"  -- excludes '"', '\\', and '?'

 xpmBase :: Int
 xpmBase = length xpmChars

 -- |Exports an 'RGBImage' as an X PixMap string suitable for writing to an
 -- @.xpm@ file
 exportXPM :: String
  -- ^The name of the generated PixMap.  It is the programmer's responsibility
  -- to ensure that this is a valid C identifier.
  -> RGBImage -- ^The image to export
  -> String
 exportXPM name rgb@(Raster dat) =
  "/* XPM */\nstatic char* " ++ name ++ "[] = {\n \"" ++ show w ++
   ' ' : show h ++ ' ' : show qty ++ ' ' : show cpp ++ "\",\n"
   ++ concatMap (\(RGBColor r g b, str) -> " \"" ++ str ++ " c #"
    ++ (hex2 r . hex2 g . hex2 b) "\",\n") (Map.toList colorMap)
   ++ concatMap (\y -> " \"" ++ concatMap (\x -> colorMap Map.! (dat ! (y, x)))
    [0..w-1] ++ (if y == h-1 then "\"\n" else "\",\n")) [0..h-1]
    -- Special casing for the last line is done so as not to confuse simplistic
    -- XPM parsers.
   ++ "};\n"
  where (h, w) = size rgb
	colors = palette rgb
	qty = Map.size colors
	cpp :: Int
	cpp = ceiling $ logBase (fromIntegral xpmBase :: Double) (fromIntegral qty)
	crosser 0 = [[]]
	crosser n = [c : str | c <- xpmChars, str <- crosser (n-1)]
	colorMap = snd $ Map.mapAccum (\(s:t) _ -> (t,s)) (crosser cpp) colors
	hex2 x = (if x < 16 then (showChar '0' .) else id) (showHex x)

 exportXBM :: String -> Maybe Coord -> Bitmap -> String
 exportXBM name hotspot xbm@(Raster dat) =
  "#define " ++ name ++ "_width " ++ show (width xbm)
   ++ "\n#define " ++ name ++ "_height " ++ show (height xbm)
   ++ case hotspot of
    Just (y, x) | inRaster xbm (y, x) -> "\n#define " ++ name ++ "_x_hot "
     ++ show x ++ "\n#define " ++ name ++ "_y_hot " ++ show y
    _ -> []
   ++ "\nstatic unsigned char " ++ name ++ "_bits[] = {\n"
   -- TODO: This needs to do a decent job of line-wrapping, and the final
   -- trailing comma should probably be removed.
   ++ concatMap (\h -> hexify [dat ! (h,i) | i <- [0 .. width xbm - 1]])
		[0 .. height xbm - 1]
   ++ "};\n"
   where hexify [] = "\n"
	 hexify b = let bits = take 8 $ take 8 b ++ repeat False
			byte = foldr (\tf a -> a*2 + fromEnum tf) 0 bits
		    in (if byte < 16 then " 0x0" else " 0x")
			++ showHex byte "," ++ hexify (drop 8 b)

 setPixels :: Raster a -> [(Coord, a)] -> Raster a
 setPixels (Raster dat) pixels = Raster $ dat // pixels

 toPixels :: Raster a -> [(Coord, a)]
 toPixels (Raster dat) = assocs dat

 fnewRaster :: (Coord -> a) -> (Int, Int) -> Raster a
 fnewRaster f (h, w) = Raster $ array grid [(c, f c) | c <- range grid]
  where grid = ((0, 0), (h-1, w-1))

 -- |@getSubRaster img ul lr@ returns the sub-image of @img@ with the
 -- upper-left corner at @ul@ and the lower-right corner at @lr@, inclusive.
 -- The sub-image's coordinates are shifted so that @ul@ becomes (0, 0).
 getSubRaster :: Raster a -> Coord -> Coord -> Raster a
 getSubRaster (Raster dat) ul@(uly, ulx) lr@(lry, lrx)
  | all (inRange $ bounds dat) [ul, lr] = Raster
     $ ixmap ((0, 0), (lry-uly, lrx-ulx)) (\(y, x) -> (y+uly, x+ulx)) dat
  | otherwise = error "Out-of-bounds coordinates passed to getSubRaster"

 mapWithCoords :: (Coord -> a -> b) -> Raster a -> Raster b
 mapWithCoords f (Raster dat) = Raster $ array (bounds dat)
  [(c, f c p) | (c, p) <- assocs dat]

 toFlatList :: Raster a -> [a]
 toFlatList = elems . toArray

 fromFlatList :: (Int, Int) -> [a] -> Raster a
 fromFlatList (h, w) = Raster . listArray ((0, 0), (h-1, w-1))

 extractMaybes :: Raster (Maybe a) -> Maybe (Raster a)
 extractMaybes (Raster dat) = sequence (elems dat) >>= return . Raster
  . listArray (bounds dat)
