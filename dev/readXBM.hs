module ReadXBM (readXBM) where
 import Data.Bits (testBit)
 import Data.Char (isSpace, toLower)
 import Data.List (isSuffixOf, find)
 import Data.Maybe
 import Numeric (readHex, readOct, readDec)
 import Raster

 readsXBM :: ReadS (String, Maybe Coord, Bitmap)
 readsXBM str = let  -- Returns [((String, Maybe Coord, Bitmap), String)]
   (defs, dat') = span ((== "#define") . head)
    $ map words $ filter (not . all isSpace) $ lines $ purgeComments str
   dat = concat dat'
   (RXState w' h' hx' hy' n') = foldl (\rxs verba -> case verba of
    ["#define", attr, val] ->
     case find (`isSuffixOf` attr) ["_width", "_height", "_x_hot", "_y_hot"] of
      Just "_width"  -> rxs {rxWidth = Just $ read val,
       rxName = Just $ take (length attr - length "_width") attr}
      Just "_height" -> rxs {rxHeight = Just $ read val}
      Just "_x_hot"  -> rxs {rxHotX = Just $ read val}
      Just "_y_hot"  -> rxs {rxHotY = Just $ read val}
      Nothing -> rxs  -- Ignore unexpected #defines
    _ -> rxs) (RXState Nothing Nothing Nothing Nothing Nothing) defs
   (_, revBits) = foldl (\(loc, bits) verbum -> case loc of
    LT -> (if '{' `elem` verbum then EQ else LT, bits)
       {- if verbum `elem` ["static", "unsigned", "char"] then (LT, bits)
	  else if "_bits" `isSuffixOf` verbum then ... -}
    EQ -> if head verbum == '}' then (GT, bits)
	  else let [(byte, rest)] =
		    if map toLower (take 2 verbum) == "0x"
		    then readHex (drop 2 verbum)
		    else if head verbum == '0' then readOct verbum
		    else readDec verbum
	       in (if '}' `elem` rest then GT else EQ, byte : bits)
    GT -> (GT, bits)) (LT, []) dat
   width = fromJust w'
   rowLen = width `div` 8 + (fromEnum $ width `mod` 8 /= 0)
   coords = concat $ zipWith
    (\(y, i) b -> [((y, i*8 + j), testBit b j) | j <- [0..7]])
    [(y, x) | y <- [0..], x <- [0..rowLen-1]] (reverse revBits :: [Int])
   name = fromJust n'
   xbm = fromRaggedPixels (fromJust h', fromJust w') False coords
  in if isJust w' && isJust h' then case (hy', hx') of
      (Just y, Just x) | inRaster xbm (y, x) -> [((name, Just (y,x), xbm), "")]
      _ -> [((name, Nothing, xbm), "")]
     else []

 data RXState = RXState {
  rxWidth, rxHeight, rxHotX, rxHotY :: Maybe Int,
  rxName :: Maybe String
 }

 purgeComments [] = []
 purgeComments ('/':'*':str) = purgeComments (scrub str)
  where scrub [] = []
	scrub ('*':'/':rest) = ' ' : rest
	scrub (_:rest) = scrub rest
 purgeComments ('/':'/':str) = purgeComments (scrub str)
  where scrub [] = []
	scrub rest@('\n':_) = rest
	scrub (_:rest) = scrub rest
 purgeComments (c:str) = c : purgeComments str

{- Things this code still needs to do:
  - Handle unexpected & malformed #defines
  - Handle unexpected tokens (especially ones in the middle of the #defines)
  - Support whitespace between the '#' and the "define"
  - Handle #defining the same attribute multiple times
  - Handle image names that differ between #defines and/or the _bits array
  - Support backslashes before newlines
  - Return the characters after the XBM definition
  - Support lack of whitespace after the opening brace
  - Support commas without spaces after them
  - Analyze the type declaration of the _bits object to make sure it's right?
  - Handle parse errors from read{Hex,Oct,Dec}
  - Handle digraphs & trigraphs
  - Handle preprocessor directives in the middle of the array
  - Support backslash-newlines inside tokens
-}
