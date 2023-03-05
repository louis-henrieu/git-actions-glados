module ByteWriter.CoWriter (
    co_argcount,
    co_nlocals,
    co_stacksize,
    co_consts,
    co_names,
    co_varnames
) where

-- For Python3.8, the first byte for each code object is an integer opcode that represents the type of the object. 
-- Here are the values of the opcodes for the main code objects:
-- 
-- co_varnames: 0x53
-- co_names: 0x73
-- co_consts: 0x63
-- co_cellvars: 0x64
-- co_freevars: 0x65
-- co_code: 0x67
-- co_lnotab: 0x6b
-- When reading a .pyc file, Python3.8 reads the first byte of each code object to determine its type and
-- how to interpret the rest of the bytes that follow.

import Data.Word
import ByteWriter.ConvertTypeBS
import ByteWriter.ObjectWriter
import Info

--getOpning :: -> Word8
--getOpning = 99
--
--getClosing :: -> Word8
--getClosing = 46
--
getListLenght :: [a] -> Int
getListLenght l = case l of
    [] -> 0
    x:xs -> 1 + (getListLenght xs)



-- ============================================= TODO ==

co_argcount :: Int -> [Word8] -- 2 bytes
co_argcount x = [0, 2]

--co_posonlyargcount :: [Word8] 
--co_posonlyargcount = getOpning:getClosing:[] --(Python 3.8 and later)
--co_kwonlyargcount :: [Word8] --(Python 3.0 and later)
--co_kwonlyargcount = getOpning:getClosing:[]
co_nlocals :: [Word8] -- 2 Bytes
co_nlocals = [0, 0]
co_stacksize :: [Word8] -- 2 Bytes
co_stacksize = [0, 2]
--co_flags :: [Word8]
--co_flags = getOpning:getClosing:[]

-- Write co_code

co_consts :: [Ast] -> [Word8] -- 4 Bytes constant numbers
co_consts ast_l = (convertInt4Bytes (fromIntegral $ getListLenght ast_l)) ++ []

co_names :: [String] -> [Word8] -- 4 Bytes constant numbers
co_names s_l = (convertInt4Bytes (fromIntegral $ getListLenght s_l)) ++ []

co_varnames :: [String] -> [Word8] -- 4 Bytes constant numbers
co_varnames s_l = (convertInt4Bytes (fromIntegral $ getListLenght s_l)) ++ []

-- co_filename :: [String] -> [Word8]
-- co_filename s_l = getOpning:getClosing:[]
-- co_name :: [String] -> [Word8]
-- co_name s_l = getOpning:getClosing:[]
-- co_firstlineno :: [String] -> [Word8]
-- co_firstlineno s_l = getOpning:getClosing:[]
