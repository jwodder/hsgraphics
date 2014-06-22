module Raster.Color where
 import Data.Ix (Ix)
 import Data.Word (Word8)
 import Numeric (readHex)

 class (Eq a) => Color a where
  black    :: a
  white    :: a
  getRGBA  :: a -> (Word8, Word8, Word8, Word8)
  getRGB   :: a -> (Word8, Word8, Word8)
  getRGB c = (r, g, b) where (r, g, b, _) = getRGBA c
  isMono   :: a -> Bool
  isMono c = c == black || c == white
  isGrey   :: a -> Bool
  isGrey c = r == g && g == b where (r, g, b, _) = getRGBA c
  isOpaque :: a -> Bool
  isOpaque c = a == 255 where (_, _, _, a) = getRGBA c
  isTransparent :: a -> Bool
  isTransparent c = a == 0 where (_, _, _, a) = getRGBA c

 class (Color a) => GreyColor a where
  fromGrey :: Word8 -> a

 class (GreyColor a) => RealColor a where
  fromRGB :: Word8 -> Word8 -> Word8 -> a

 class (RealColor a) => AlphaColor a where
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a

---------------------------------------

 fromBit :: Color a => Bool -> a
 fromBit True  = black
 fromBit False = white

 toMono :: (Color a, Color b) => a -> Maybe b
 toMono c = if c == black then Just black
	    else if c == white then Just white
	    else Nothing

 toGrey :: (Color a, GreyColor b) => a -> Maybe b
 toGrey c | isGrey c = Just (fromGrey r) where (r, _, _, _) = getRGBA c
 toGrey _  = Nothing

 toRGB :: (Color a, RealColor b) => a -> b
 toRGB c = fromRGB r g b where (r, g, b) = getRGB c

 toRGBA :: (Color a, AlphaColor b) => a -> b
 toRGBA c = fromRGBA r g b a where (r, g, b, a) = getRGBA c

 readHex6 :: RealColor a => ReadS a
 readHex6 txt = do let (pre, post) = splitAt 6 txt
		   (rgb, rest) <- readHex pre
		   return (toRGB (toEnum rgb :: RGBColor), rest ++ post)

 readHex8 :: AlphaColor a => ReadS a
 readHex8 txt = do let (pre, post) = splitAt 8 txt
		   (rgba, rest) <- readHex pre
		   return (toRGBA (toEnum rgba :: RGBAColor), rest ++ post)

---------------------------------------

 instance Color Bool where
  black = True
  white = False
  getRGBA True  = (0, 0, 0, 255)
  getRGBA False = (255, 255, 255, 255)

---------------------------------------

 instance Color Word8 where
  black = 0
  white = 255
  getRGBA c = (c, c, c, 255)

 instance GreyColor Word8 where
  fromGrey = id

---------------------------------------

 -- |A color specified as a red value, green value, and blue value, in that
 -- order.  All values are in the range @[0..255]@.
 data RGBColor = RGBColor Word8 Word8 Word8
  deriving (Eq, Ord, Read, Show, Bounded, Ix)

 instance Enum RGBColor where
  toEnum x | 0 <= x && x <= 0xFFFFFF =
   RGBColor (fromIntegral r) (fromIntegral g) (fromIntegral b)
   where (rg, b) = divMod x 256
	 (r', g) = divMod rg 256
	 r = r' `mod` 256
  toEnum _ = undefined
  fromEnum (RGBColor r g b) = fromIntegral r * 65536 + fromIntegral g * 256
						     + fromIntegral b
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

 instance Color RGBColor where
  black = RGBColor 0 0 0
  white = RGBColor 255 255 255
  getRGB  (RGBColor r g b) = (r, g, b)
  getRGBA (RGBColor r g b) = (r, g, b, 255)

 instance GreyColor RGBColor where
  fromGrey gr = RGBColor gr gr gr

 instance RealColor RGBColor where
  fromRGB = RGBColor

---------------------------------------

 data RGBAColor = RGBAColor Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Read, Show, Bounded, Ix)

 instance Enum RGBAColor where
  toEnum x | 0 <= x && x <= 0xFFFFFFFF =
   RGBAColor (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
   where (rgb, a) = divMod x 256
	 (rg, b) = divMod rgb 256
	 (r', g) = divMod rg 256
	 r = r' `mod` 256
  toEnum _ = undefined
  fromEnum (RGBAColor r g b a) = fromIntegral r * 16777216
			       + fromIntegral g * 65536
			       + fromIntegral b * 256
			       + fromIntegral a
  enumFrom x = enumFromTo x maxBound
  enumFromThen x y = enumFromThenTo x y (if y >= x then maxBound else minBound)

 instance Color RGBAColor where
  black = RGBAColor 0 0 0 255
  white = RGBAColor 255 255 255 255
  getRGB  (RGBAColor r g b _) = (r, g, b)
  getRGBA (RGBAColor r g b a) = (r, g, b, a)

 instance GreyColor RGBAColor where
  fromGrey gr = RGBAColor gr gr gr 255

 instance RealColor RGBAColor where
  fromRGB r g b = RGBAColor r g b 255

 instance AlphaColor RGBAColor where
  fromRGBA = RGBAColor
