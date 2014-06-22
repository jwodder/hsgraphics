module Raster.Palette (module Raster.Palette, black, white) where
 import Raster.Color

 blue, green, cyan, purple, brown, red, magenta, orange, yellow
  :: RealColor a => a
 blue    = fromRGB    0    0 0xFF
 green   = fromRGB    0 0xFF    0
 cyan    = fromRGB    0 0xFF 0xFF
 purple  = fromRGB 0x80    0 0x80
 brown   = fromRGB 0x99 0x66 0x33
 red     = fromRGB 0xFF    0    0
 magenta = fromRGB 0xFF    0 0xFF
 orange  = fromRGB 0xFF 0x80    0
 yellow  = fromRGB 0xFF 0xFF    0

 grey :: GreyColor a => a
 grey = fromGrey 0xCC
