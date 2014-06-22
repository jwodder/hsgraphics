-- items in input file: width height camera UL-screen-corner xEdge yEdge object*

import Control.Monad (ap, guard, liftM2, liftM3, liftM4)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Numeric
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as Buf (writeFile)
import Text.ParserCombinators.ReadP
import Vec3
import Raster (fnewRaster, exportBMP)
import Raster.Color
import Raster.Palette
import Raytrace

main :: IO ()
main = do
 (width, height, camera, (cor, xEdge, yEdge), scene)
  <- getArgs >>= readArgv >>= readScene . head
 putStrLn "Parsed; now tracing..."
 Buf.writeFile "tracer.bmp" {- IMPROVE THIS -} $ exportBMP $ fnewRaster
  (\(y, x) -> toRGB $ traceRay scene 6 (camera, cor <+> scale (idiv x width) xEdge <+> scale (idiv y height) yEdge <-> camera))
  (height, width)

idiv :: (Integral a, Integral b, Fractional c) => a -> b -> c
idiv x y = fromIntegral x / fromIntegral y

type Screen a = (Vec3 a, Vec3 a, Vec3 a)  -- corner, x-edge, y-edge

readArgv :: [FilePath] -> IO [String]
readArgv [] = getContents >>= return . (: [])
readArgv xs = mapM (\f -> if f == "-" then getContents else readFile f) xs

readScene :: String -> IO (Int, Int, Vec3 Double, Screen Double, Scene Double)
readScene txt = case [d | (d,r) <- readP_to_S readScene' uncommed,
			  ("", "") <- lex r] of
 [dat] -> return dat
 []    -> fail "Could not parse input"
 _     -> fail "Input is ambiguous (How'd you do that?)"
 where uncommed = unlines $ map (takeWhile (/= '#')) $ lines txt
       readScene' = do skipSpaces
		       w <- readS_to_P readDec
		       skipSpaces1
		       h <- readS_to_P readDec
		       skipSpaces1
		       cam <- readVec
		       skipSpaces1
		       screen <- readScreen
		       skipSpaces1
		       scene <- most readObject
		       return (w, h, cam, screen, scene)

readSFloat :: ReadP Double
readSFloat = readS_to_P $ readSigned readFloat

readVec :: ReadP (Vec3 Double)
readVec = fmap Vec3 $ readS_to_P reads

readScreen :: ReadP (Screen Double)
readScreen = liftM3 (,,) readVec readVec readVec

readObject :: ReadP (Object Double)
readObject = readObj' +++ readLight
 where readObj' = do o <- readSphere +++ readPlane +++ readTriangle
			   +++ readPGram +++ readCylinder +++ readRegular
		     skipSpaces
		     readToken ":"
		     m <- readMatter
		     return (o,m)

readSphere :: ReadP (Intersectable Double)
readSphere = readToken "sphere" >> liftM2 sphere readVec readSFloat

readPlane :: ReadP (Intersectable Double)
readPlane = readToken "plane" >> liftM2 plane readVec readSFloat

readTriangle :: ReadP (Intersectable Double)
readTriangle = readToken "triangle" >> liftM3 triangle readVec readVec readVec

readPGram :: ReadP (Intersectable Double)
readPGram = readToken "parallelogram" >> liftM3 parallelogram readVec readVec readVec

readCylinder :: ReadP (Intersectable Double)
readCylinder = do readToken "cylinder"
		  axOrig <- readVec
		  axDir  <- readVec
		  r      <- readSFloat
		  return $ cylinderShell (axOrig, axDir) r

readRegular :: ReadP (Intersectable Double)
readRegular = readToken "regular" >> liftM4 regularPoly readVec readVec readVec (skipSpaces >> readS_to_P readDec)

readLight :: ReadP (Object Double)
readLight = readToken "light" >> liftM3 mkLight readVec readSFloat readColor

readMatter :: ReadP (Material Double)
readMatter = do c <- readColor; matter1 c +++ matter2 c
 where matter1 c = do d <- readSFloat
		      r <- readSFloat 
		      f <- optional' readSFloat
		      return $ Material c d (1-d) r (fromMaybe 0 f) Nothing
       matter2 c = fmap (foldl (flip id) (Material c 0.2 0.8 0 0 Nothing))
			(most $ skipSpaces1 >> ap (readDiff +++ readSpec +++ readRefl +++ readRefr) readSFloat)

readDiff = (string "diffuse=" +++ string "diff=" +++ string "d=")
 >> return (\d mat -> mat {diffusion=d})

readSpec = (string "specular=" +++ string "spec=" +++ string "s=")
 >> return (\d mat -> mat {specularity=d})

readRefl = (string "reflect=" +++ string "refl=" +++ string "r=")
 >> return (\d mat -> mat {reflection=d})

readRefr = (string "refract=" +++ string "refr=" +++ string "nr=" +++ string "f=") >> return (\d mat -> mat {refracRatio=d})

readColor :: ReadP (RTColor Double)
readColor = readEntry [("blue", blue), ("green", green), ("cyan", cyan),
		       ("purple", purple), ("brown", brown), ("red", red),
		       ("magenta", magenta), ("orange", orange), ("grey", grey),
		       ("yellow", yellow), ("black", black), ("white", white)]
		      (readS_to_P lex)
	     +++ (char 'x' >> readS_to_P readHex6)
	     +++ fmap (\(r, g, b) -> rtcolor r g b) (readS_to_P reads)

-- |@most@ and @most1@ parse as many instances of the given parser as
-- possible, without creating any \"branches\" for stopping early.
most, most1 :: ReadP a -> ReadP [a]
most  p = most1 p <++ return []
most1 p = liftM2 (:) p (most p)

readToken :: String -> ReadP ()
readToken tok = readS_to_P lex >>= guard . (== tok)

skipSpaces1 :: ReadP ()
skipSpaces1 = munch1 isSpace >> return ()

readEntry :: Eq a => [(a,b)] -> ReadP a -> ReadP b
readEntry dict f = do a <- f; Just b <- return $ lookup a dict; return b

optional' :: ReadP a -> ReadP (Maybe a)
optional' f = fmap Just f +++ return Nothing
