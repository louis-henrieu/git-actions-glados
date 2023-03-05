module ByteWriter.ConvertTypeBS (
    convertInt,
    convertInt4Bytes
) where

import Data.Bits
import Data.Binary.IEEE754 as DBI
import Data.Binary as BIN
import Data.Word as W
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.Char (ord)
import qualified Data.ByteString.Builder as BUI
--import qualified Data.ByteString.Lazy as BL

--import Conversion

getEnd :: Word8
getEnd = 0

--convertFloatIEEE754 :: Float -> W.Word32
--convertFloatIEEE754 x = DBI.floatToWord x
--
--floatToBytesLE :: Float -> [Word8]
--floatToBytesLE x = let w = floatToWord x
--                       b1 = fromIntegral (w .&. 0xFF)
--                       b2 = fromIntegral ((w `shiftR` 8) .&. 0xFF)
--                       b3 = fromIntegral ((w `shiftR` 16) .&. 0xFF)
--                       b4 = fromIntegral ((w `shiftR` 24) .&. 0xFF)
--                   in [b1, b2, b3, b4]

--convertWord32ToWord8 :: Word32 -> [Word8]
--convertWord32ToWord8 word = 

-- convertInt :: Int -> W.Word32
-- convertInt x = DBI.intToWord x

-- Conversion (Int) (W.Word8) => 

--convertInt :: Int -> [W.Word8]
--convertInt x = case x of
--    0 -> []
--    x | x > 255 -> 
--    x -> (fromIntegral x) : convertInt (0)

intToBytes :: Int -> B.ByteString
intToBytes n = BUI.toLazyByteString $ BUI.word32BE (fromIntegral n)

convertInt4Bytes :: Int -> [W.Word8]
convertInt4Bytes x = B.unpack $ intToBytes x

convertInt :: Int -> [W.Word8]
convertInt x = if x < 0 
    then 255:(B.unpack $ intToBytes x)
    else 0:(B.unpack $ intToBytes x)

floatToBytes :: Float -> B.ByteString
floatToBytes f = BUI.toLazyByteString $ BUI.floatLE f

convertFloat :: Float -> [W.Word8]
convertFloat x = B.unpack $ floatToBytes x