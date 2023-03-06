module ByteWriter.ObjectWriter (
    createIntObj,
    createFloatObj
) where

import ByteWriter.ConvertTypeBS
import Data.Word

--data ObjectType
--   = NULL               -- '0'
--   | NONE               -- 'N'
--   | FALSE              -- 'F'
--   | TRUE               -- 'T'
--   | STOPITER           -- 'S'
--   | ELLIPSIS           -- '.'
--   | INT                -- 'i'
--
--   | INT64              -- 'I' INT64 is deprecated. It is not,
--                        -- generated anymore, and support for reading it
--                        -- will be removed in Python 3.4.
--
--   | FLOAT              -- 'f'
--   | BINARY_FLOAT       -- 'g'
--   | COMPLEX            -- 'x'
--   | BINARY_COMPLEX     -- 'y'
--   | LONG               -- 'l'
--   | STRING             -- 's'
--   | TUPLE              -- '('
--   | LIST               -- '['
--   | DICT               -- '{'
--   | CODE               -- 'c'
--   | UNICODE            -- 'u'
--   | UNKNOWN            -- '?'
--   | SET                -- '<'
--   | FROZENSET          -- '>'
--   deriving (Eq, Ord, Show)

-- | createIntObj :: Int -> [Word8]
-- creates a viable Int object
-- 105 -> 'i' -> ID for Int
-- 4 -> number of abs value byte alowed to the integer
--
createIntObj :: Int -> [Word8]
createIntObj x = 105:(convertInt4Bytes x)

createFloatObj :: Float -> [Word8]
createFloatObj x = 102:(convertFloat x)