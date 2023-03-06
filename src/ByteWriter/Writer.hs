module ByteWriter.Writer where

import Data.ByteString.Lazy as B
import Data.ByteString.Char8
import Data.Word as W

import Data.List.Split
import Data.Char
import Data.Map

import ByteWriter.CoWriter

opcodeList :: Map String Word8
opcodeList = Data.Map.fromList [
   ("POP_TOP", 1),
   ("ROT_TWO", 2),
   ("ROT_THREE", 3),
   ("DUP_TOP", 4),
   ("DUP_TOP_TWO", 5),
   ("NOP", 9),
   ("UNARY_POSITIVE", 10),
   ("UNARY_NEGATIVE", 11),
   ("UNARY_NOT", 12),
   ("UNARY_INVERT", 15),
   ("BINARY_POWER", 19),
   ("BINARY_MULTIPLY", 20),
   ("BINARY_MODULO", 22),
   ("BINARY_ADD", 23),
   ("BINARY_SUBTRACT", 24),
   ("BINARY_SUBSCR", 25),
   ("BINARY_FLOOR_DIVIDE", 26),
   ("BINARY_TRUE_DIVIDE", 27),
   ("INPLACE_FLOOR_DIVIDE", 28),
   ("INPLACE_TRUE_DIVIDE", 29),
   ("STORE_MAP", 54),
   ("INPLACE_ADD", 55),
   ("INPLACE_SUBTRACT", 56),
   ("INPLACE_MULTIPLY", 57),
   ("INPLACE_MODULO", 59),
   ("STORE_SUBSCR", 60),
   ("DELETE_SUBSCR", 61),
   ("BINARY_LSHIFT", 62),
   ("BINARY_RSHIFT", 63),
   ("BINARY_AND", 64),
   ("BINARY_XOR", 65),
   ("BINARY_OR", 66),
   ("INPLACE_POWER", 67),
   ("GET_ITER", 68),
   ("STORE_LOCALS", 69),
   ("PRINT_EXPR", 70),
   ("LOAD_BUILD_CLASS", 71),
   ("YIELD_FROM", 72),
   ("INPLACE_LSHIFT", 75),
   ("INPLACE_RSHIFT", 76),
   ("INPLACE_AND", 77),
   ("INPLACE_XOR", 78),
   ("INPLACE_OR", 79),
   ("BREAK_LOOP", 80),
   ("WITH_CLEANUP", 81),
   ("RETURN_VALUE", 83),
   ("IMPORT_STAR", 84),
   ("YIELD_VALUE", 86),
   ("POP_BLOCK", 87),
   ("END_FINALLY", 88),
   ("POP_EXCEPT", 89),
   -- (HAVE_ARGUMENT, 90),
   ("STORE_NAME", 90),
   ("DELETE_NAME", 91),
   ("UNPACK_SEQUENCE", 92),
   ("FOR_ITER", 93),
   ("UNPACK_EX", 94),
   ("STORE_ATTR", 95),
   ("DELETE_ATTR", 96),
   ("STORE_GLOBAL", 97),
   ("DELETE_GLOBAL", 98),
   ("LOAD_CONST", 100),
   ("LOAD_NAME", 101),
   ("BUILD_TUPLE", 102),
   ("BUILD_LIST", 103),
   ("BUILD_SET", 104),
   ("BUILD_MAP", 105),
   ("LOAD_ATTR", 106),
   ("COMPARE_OP", 107),
   ("IMPORT_NAME", 108),
   ("IMPORT_FROM", 109),
   ("JUMP_FORWARD", 110),
   ("JUMP_IF_FALSE_OR_POP", 111),
   ("JUMP_IF_TRUE_OR_POP", 112),
   ("JUMP_ABSOLUTE", 113),
   ("POP_JUMP_IF_FALSE", 114),
   ("POP_JUMP_IF_TRUE", 115),
   ("LOAD_GLOBAL", 116),
   ("CONTINUE_LOOP", 119),
   ("SETUP_LOOP", 120),
   ("SETUP_EXCEPT", 121),
   ("SETUP_FINALLY", 122),
   ("LOAD_FAST", 124),
   ("STORE_FAST", 125),
   ("DELETE_FAST", 126),
   ("RAISE_VARARGS", 130),
   ("CALL_FUNCTION", 131),
   ("MAKE_FUNCTION", 132),
   ("BUILD_SLICE", 133),
   ("MAKE_CLOSURE", 134),
   ("LOAD_CLOSURE", 135),
   ("LOAD_DEREF", 136),
   ("STORE_DEREF", 137),
   ("DELETE_DEREF", 138),
   ("CALL_FUNCTION_VAR", 140),
   ("CALL_FUNCTION_KW", 141),
   ("CALL_FUNCTION_VAR_KW", 142),
   ("SETUP_WITH", 143),
   ("EXTENDED_ARG", 144),
   ("LIST_APPEND", 145),
   ("SET_ADD", 146),
   ("MAP_ADD", 147)
   -- (EXCEPT_HANDLER, 257)
   ]



getWord8MagicNumPython38 :: [W.Word8]
getWord8MagicNumPython38 = [85, 13, 13, 10]

getWord8BitField :: [W.Word8]
getWord8BitField = [0, 0, 0, 0]

-- getWord8FileSize :: [W.Word8]
-- getWord8FileSize = []

getWord8FakeTimeStamp :: [W.Word8]
getWord8FakeTimeStamp = [98,56,5,14]

getMagicNumPython38 :: B.ByteString
getMagicNumPython38 = B.pack [85, 13, 13, 10]

appendListToList :: [a] -> [a] -> [a]
appendListToList a b = case a of
    [] -> b
    x:xs -> x : appendListToList (xs) b

myIsNumber :: String -> Bool
myIsNumber l = case l of
    [] -> True
    x:xs | (isDigit x) == True -> myIsNumber xs
    _ -> False

convertInstructToInt :: [String] -> [Word8]
convertInstructToInt l = case l of
    [] -> []
    x:xs | myIsNumber x -> ((read x) :: Word8) : convertInstructToInt xs
    x:xs -> opcodeList ! x : convertInstructToInt xs

 -- appendListToList (splitOn " " x) (buildByteList xs)
buildByteList :: [String] -> [Word8]
buildByteList bytelist = case bytelist of
    [] -> []
    x:xs -> appendListToList (convertInstructToInt (splitOn " " x)) (buildByteList xs)

-- ================================================================ --

-- | writeMagicNum String -> IO ()
-- String -> Filepath
-- 
-- Writes the MagicNum for Python3.8
--
writeMagicNum :: String -> IO ()
writeMagicNum filePath = B.writeFile filePath (getMagicNumPython38)

--writeHeader :: String -> IO ()
--writeHeader file = B.writeFile file (B.pack (getWord8MagicNumPython38 ++ ))

-- | writeMagicNum String -> [Word8] -> IO ()
-- String -> Filepath
-- [Word8] -> list of instruction as Byte
--
-- append the main body of the Bytecode File
--
appendByteList :: String -> [Word8] -> IO ()
appendByteList file byteList = B.appendFile file (B.pack byteList)

-- ========================================================= --

-- | writeAllBytes String -> [String] -> IO ()
-- String -> file path
-- [String] -> instruction list
--
-- Write every Bytes needed at once
--
writeAllBytes :: String -> [String] -> IO ()
writeAllBytes file instructionList = B.writeFile file (B.pack (getWord8MagicNumPython38 ++ getWord8FakeTimeStamp ++ (co_argcount 0) ++ co_kwonlyargcount ++ co_nlocals ++ co_stacksize ++ co_flags ++ (buildByteList instructionList) ++ (co_consts []) ++ (co_names []) ++ (co_varnames []) ++ (co_filename "") ++ (co_name "") ++ (co_firstlineno 0) ++ (co_lnotab []) ++ [0, 0, 0, 0] ))
