{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ReadXBM2 (readsXBM) where
 import Control.Monad (liftM2, guard)
 import Data.Bits (testBit)
 import Data.Char
 import Data.List (isSuffixOf)
 import Text.ParserCombinators.ReadP
 import Parsing.ReadP (most, most1, cint)
 import Raster hiding (height, width)

 readsXBM :: ReadS (String, Maybe Coord, Bitmap)
 readsXBM = readP_to_S $ do
  most $ manyTill space_nonl $ char '\n'
  defs <- sepBy (intDefine <++ otherDef) (many $ manyTill space_nonl $ char '\n')
  let defines = reverse [x | Just x <- defs]
  cspace
  optional $ string "static"   >> cspace1
  optional $ string "unsigned" >> cspace1
  string "char" >> cspace1
  bits_name <- cident
  guard $ isSuffixOf "_bits" bits_name
  let name = take (length bits_name - 5) bits_name
  Just width  <- return $ lookup (name ++ "_width")  defines
  Just height <- return $ lookup (name ++ "_height") defines
  cspace >> lbracket >> cspace >> (optional $ cspace>>cint>>cspace) >> rbracket
  cspace >> char '=' >> cspace >> lbrace >> cspace
  bits <- sepBy1 cint $ cspace >> char ',' >> cspace
  optional $ cspace >> char ',' >> cspace
  cspace >> rbrace >> cspace >> char ';' >> cspace
  let rowLen = (width + 7) `div` 8  -- bytes per row
      xbmate [] = []
      xbmate bs = [testBit b i | b <- take rowLen bs, i <- [0..7]]
		   : xbmate (drop rowLen bs)
      xbm = fromRaggedList (height, width) False $ xbmate bits
      hotspot = do xHot <- lookup (name ++ "_x_hot") defines
		   yHot <- lookup (name ++ "_y_hot") defines
		   if 0 <= xHot && xHot < width && 0 <= yHot && yHot < height
		      then return (yHot, xHot) else Nothing
  return (name, hotspot, xbm)

 cspace, cspace1, cspace' :: ReadP ()
 cspace  = most  cspace' >> return ()
 cspace1 = most1 cspace' >> return ()
 cspace' = lineComment <++ blockComment <++ (satisfy isSpace >> return ())
  -- Replacing the "satisfy" expression above with "skipSpaces" causes the code
  -- not to terminate, for some reason.

 blockComment :: ReadP ()
 blockComment = char '/' >> nonl >> char '*'
  >> manyTill get (char '*' >> nonl >> char '/') >> return ()

 lineComment :: ReadP ()
 lineComment = char '/' >> nonl >> char '/'
  >> manyTill (nonl <++ (get >> return ())) (do '\n':_ <- look; return ())
  >> return ()

 nonl :: ReadP ()
 nonl = (backslash >> char '\n' >> return ()) <++ return ()

 cident :: ReadP String
 cident = liftM2 (:) (satisfy $ \c -> isAlpha    c || c == '_')
		     (munch   $ \c -> isAlphaNum c || c == '_')

 space_nonl :: ReadP ()
 space_nonl = nonl >> lineComment <++ blockComment
  <++ (satisfy (\c -> isSpace c && c /= '\n') >> return ()) >> nonl

 intDefine :: ReadP (Maybe (String, Int))
 intDefine = do skipSpaces
		pound
		skipMany space_nonl
		string "define"
		skipMany1 space_nonl
		name <- cident
		skipMany1 space_nonl
		val <- cint
		skipMany space_nonl
		char '\n'
		return $ Just (name, val)

 otherDef :: ReadP (Maybe a)
 otherDef = skipSpaces >> pound
  >> manyTill (space_nonl <++ (satisfy (/= '\n') >> return ())) (char '\n')
  >> return Nothing

 pound, lbracket, rbracket, lbrace, rbrace, backslash :: ReadP Char
 pound     = string "#" +++ string "%:" +++ string "??=" >> return '#'
 lbracket  = string "[" +++ string "<:" +++ string "??(" >> return '['
 rbracket  = string "]" +++ string ":>" +++ string "??)" >> return ']'
 lbrace    = string "{" +++ string "<%" +++ string "??<" >> return '{'
 rbrace    = string "}" +++ string "%>" +++ string "??>" >> return '}'
 backslash = string "\\" +++ string "??/" >> return '\\'

{- Things this code still needs to do:
 * Handle preprocessor directives in the middle of the array
 * Support backslash-newlines inside tokens
-}
