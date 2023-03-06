module ByteWriter.CoWriter (
    co_argcount,
    co_kwonlyargcount,
    co_nlocals,
    co_stacksize,
    co_flags,
    co_consts,
    co_names,
    co_varnames,
    co_filename,
    co_name,
    co_firstlineno,
    co_lnotab
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

co_argcount :: Int -> [Word8] -- 4 bytes
co_argcount x = [0, 0, 0, 0]

--co_posonlyargcount :: [Word8] 
--co_posonlyargcount = getOpning:getClosing:[] --(Python 3.8 and later)

co_kwonlyargcount :: [Word8] --(Python 3.0 and later)
co_kwonlyargcount = [0, 0, 0, 0]

co_nlocals :: [Word8] -- 4 Bytes
co_nlocals = [0, 0, 0, 0]
co_stacksize :: [Word8] -- 4 Bytes
co_stacksize = [0, 0, 0, 2]

co_flags :: [Word8]
co_flags = [0, 0, 0, 0]

-- Write co_code

co_consts_object :: [Ast] -> [Word8]
co_consts_object ast_l = case ast_l of
    [] -> []
    (IntegerAst x):l -> (createIntObj x) ++ (co_consts_object l)
    (FloatAst x):l -> (createFloatObj x) ++ (co_consts_object l)
    _:l -> (co_consts_object l)

co_consts :: [Ast] -> [Word8] -- 4 Bytes constant numbers
co_consts ast_l = 40:(convertInt4Bytes (fromIntegral $ getListLenght ast_l)) ++ (co_consts_object ast_l)

co_names :: [String] -> [Word8] -- 4 Bytes constant numbers
co_names s_l = 40:(convertInt4Bytes (fromIntegral $ getListLenght s_l)) ++ []

co_varnames :: [String] -> [Word8] -- 4 Bytes constant numbers
co_varnames s_l = 40:(convertInt4Bytes (fromIntegral $ getListLenght s_l)) ++ []

co_filename :: String -> [Word8]
co_filename s_l = 115:(convertInt4Bytes (fromIntegral $ getListLenght s_l))

co_name :: String -> [Word8]
co_name s_l = 115:(convertInt4Bytes (fromIntegral $ getListLenght s_l))

co_firstlineno :: Int -> [Word8] --  Can't do it rn
co_firstlineno x = (convertInt4Bytes x)

co_lnotab :: [(Int, Int)] -> [Word8]
co_lnotab x = (fromIntegral $ getListLenght x):[]