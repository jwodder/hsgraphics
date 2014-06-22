module Raster.Palette.HTML (module Raster.Palette.HTML, black, white) where
 import Raster.Color

 -- The following colors are taken from the HTML 4 and CSS 2.1 specifications.
 -- Note that some of them conflict with the colors in Raster.Palette.

 navy, blue, green, teal, lime, aqua, maroon, purple, olive, red, fuchsia,
  orange, yellow :: RealColor a => a
 navy    = fromRGB    0    0 0x80
 blue    = fromRGB    0    0 0xFF
 green   = fromRGB    0 0x80    0
 teal    = fromRGB    0 0x80 0x80
 lime    = fromRGB    0 0xFF    0
 aqua    = fromRGB    0 0xFF 0xFF
 maroon  = fromRGB 0x80    0    0
 purple  = fromRGB 0x80    0 0x80
 olive   = fromRGB 0x80 0x80    0
 red     = fromRGB 0xFF    0    0
 fuchsia = fromRGB 0xFF    0 0xFF
 orange  = fromRGB 0xFF 0xA5    0
 yellow  = fromRGB 0xFF 0xFF    0

 gray, silver :: GreyColor a => a
 gray   = fromGrey 0x80
 silver = fromGrey 0xC0
