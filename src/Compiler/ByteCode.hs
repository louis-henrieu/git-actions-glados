--
-- {Python Byte Code} as Functionnal Language
-- AST -> to ByteCode
-- Case of Pattern Matching -> not IF
-- functionnal only goes downwards
-- MODE DESASSAMBLEUR -> Strace like but for Byte-Code
--

module Compiler.ByteCode where

exampleFunction :: Int -> Int -> Int
exampleFunction a b = a + b
-- 0 LOAD_FAST    0 (a)
-- 2 LOAD_FAST    1 (b)
-- 4 BINARY_ADD
-- 6 RETURN_VALUE
-- in theory every single instruction is designated by a byte

-- the there above bytcode is represented as [124, 0, 124, 1, 23, 0, 83, 0]
-- 124 -> LOAD_FAST
-- 0 & 1 -> a & b
-- 23 -> BINARY_ADD
-- 83 -> RETURN_VALUE
-- the numbers the instruction correspond to place of said instruction.


