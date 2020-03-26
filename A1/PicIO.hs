module PicIO where

-- Tools for loading and saving images from/to files so you can use an image
-- viewer to see results.

import Control.Monad (replicateM)
import Data.Array
import Data.Char (chr, ord)
import Data.List (intercalate)
import Data.Word
import System.IO
import Text.Parsec
import Text.Parsec.String

import Quadtree (picToQuadtree, quadtreeToPic)
import QuadtreeDef

-- Round-trip picture -> quadtree under threshold and depth cap -> picture.
roundTrip :: Array (Int,Int) Word8
          -> Word8
          -> Int
          -> Array (Int,Int) Word8
roundTrip inp = roundTripWith picToQuadtree quadtreeToPic inp

-- Like roundTrip but use given functions instead of picToQuadtree and
-- quadtreeToPic.
roundTripWith :: (Word8 -> Int -> Array (Int,Int) Word8 -> Quadtree)
              -> (Quadtree -> Array (Int,Int) Word8)
              -> Array (Int,Int) Word8
              -> Word8
              -> Int
              -> Array (Int,Int) Word8
roundTripWith pq qp pic threshold depthCap = qp (pq threshold depthCap pic)

-- Like roundTrip but load picture from file and save picture to file.
-- Example usages:
--   loadSave "test.pgm" 50 10 "out-50-10.pgm"
--   loadSave "test.pgm" 10 5 "out-10-5.pgm"
loadSave :: FilePath  -- load-from filename
         -> Word8     -- threshold
         -> Int       -- depth cap
         -> FilePath  -- save-to filename
         -> IO ()

-- Load picture from file.
load :: FilePath -> IO (Array (Int,Int) Word8)

-- Save picture to file.
save :: Array (Int,Int) Word8 -> FilePath -> IO ()


-- All you need to know is in the above. Below are gory implemention details.

loadSave inpfile threshold depthCap outfile =
    load inpfile
    >>= \pic ->
    let newPic = roundTrip pic threshold depthCap in
    save newPic outfile

load filepath =
    withBinaryFile filepath ReadMode $ \h ->
      hGetContents h
      >>= \str -> case parse pgmParser filepath str of
        Left err -> error (show err)
        Right pic -> return pic

save pic filepath = withBinaryFile filepath WriteMode $ \h -> hPutStr h (picToPGM pic)

picToPGM :: Array (Int,Int) Word8 -> String
picToPGM pic = "P5\n"
               ++ show width ++ " " ++ show height ++ "\n255\n"
               ++ [chr (fromIntegral (pic!(i,j))) | j <- [y..y1], i <- [x..x1]]
                  -- Note row-major order for PGM, e.g., (0,0), (1,0), (2,0),...
  where
    ((x,y), (x1,y1)) = bounds pic
    width = x1 - x + 1
    height = y1 - y + 1

pgmParser :: Parser (Array (Int,Int) Word8)
pgmParser =
    string "P5" >> whitespace
    >> parseIntegral <* whitespace
    >>= \width -> parseIntegral <* whitespace
    >>= \height -> parseIntegral <* space
    >>= \maxGrey -> replicateM (width * height) (fmap (fromIntegral . ord) anyChar)
    >>= \pixels ->
    let pixelXYs = zip [(x,y) | y <- [0 .. height-1], x <- [0 .. width-1]] pixels
        pic = array ((0,0), (width-1, height-1)) pixelXYs
    in
    return pic

parseIntegral :: (Integral a, Read a) => Parser a
parseIntegral = fmap read (many1 digit)

whitespace = spaces >> optional comment
comment = char '#' >> manyTill (satisfy ('\n' /=)) (char '\n') >> whitespace