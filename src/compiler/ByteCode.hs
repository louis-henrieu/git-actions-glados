--
-- {Python Byte Code} as Functionnal Language
-- AST -> to ByteCode
-- Case of Pattern Matching -> not IF
-- functionnal only goes downwards
-- MODE DESASSAMBLEUR -> Strace like but for Byte-Code
--

module ByteCode where

exampleFunction :: a -> b -> c
exampleFunction a b = a + b
-- LOAD_FAST a
-- LOAD_FAST b
-- BINARY_ADD
-- RETURN_VALUE