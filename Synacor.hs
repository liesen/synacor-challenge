{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.List (intercalate)
import Debug.Trace

import Prelude hiding (readFile)

data Machine = Machine
    { mem :: Array Word16 Word16
    , reg :: Array Word16 Word16
    , stack :: [Word16]
    , pc :: Word16
    }

instance Show Machine where
    show Machine{..} = concat ["[", show pc, ",", show reg, ",", show (take 3 stack), "]"]

-- Test input
testinput = runPut $ mapM_ putWord16le [9,32768,32769,4,19,32768,0]

data Val = Lit Word16
         | Reg Word16

instance Show Val where
    show (Lit x) = "!" ++ show x
    show (Reg x) = "@" ++ show x

step :: Bool -> Machine -> Maybe (Machine, [Char])
step debugFlag m@Machine{..} = 
    let op = mem ! pc
    in debugShow (m, op) $ case op of
        -- halt: 0
        --   stop execution and terminate the program
        0 -> debug "halt" Nothing
        -- set: 1 a b
        --   set register <a> to the value of <b>
        1 -> 
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
            in debug (intercalate " " ["set", show a, show b]) $
                Just (m{reg = reg // [(a, b)], pc = pc + 3}, [])
        -- push: 2 a
        --   push <a> onto the stack
        2 ->
            let a = v (mem ! (pc + 1))
            in debug (intercalate " " ["push", show a]) $ Just (m{stack = a:stack, pc = pc + 2}, [])
        -- pop: 3 a
        --   remove the top element from the stack and write it into <a>; empty stack = error
        3 ->
            let a = r (mem ! (pc + 1))
                s:ss = stack
            in debug (intercalate " " ["pop", show a]) $ Just (m{stack = ss, reg = reg // [(a, s)], pc = pc + 2}, [])
        -- eq: 4 a b c
        --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        4 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in debug (intercalate " " ["eq", show a, show b, show c]) $ Just (m{reg = reg // [(a, if b == c then 1 else 0)], pc = pc + 4}, [])
        -- gt: 5 a b c
        --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        5 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, if b > c then 1 else 0)], pc = pc + 4}, [])
        -- jmp: 6 a
        --   jump to <a>
        6 ->
            let a = v (mem ! (pc + 1))
            in debug (intercalate " " ["jmp", show a]) $ Just (m{pc = a}, [])
        -- jt: 7 a b
        --   if <a> is nonzero, jump to <b>
        7 ->
            let a = v (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
            in debug (intercalate " " ["jt", show a, show b]) $ Just (m{pc = if a /= 0 then b else pc + 3}, [])
        -- jf: 8 a b
        --   if <a> is zero, jump to <b>
        8 ->
            let a = v (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
            in debug (intercalate " " ["jf", show a, show b]) $ Just (m{pc = if a == 0 then b else pc + 3}, [])
        -- add: 9 a b c
        --   assign into <a> the sum of <b> and <c> (modulo 32768)
        9 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, (b + c) `mod` 32768)], pc = pc + 4}, [])
        -- mult: 10 a b c
        --   store into <a> the product of <b> and <c> (modulo 32768)
        10 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, (b * c) `mod` 32768)], pc = pc + 4}, [])
        -- mod: 11 a b c
        --   store into <a> the remainder of <b> divided by <c>
        11 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, b `mod` c)], pc = pc + 4}, [])
        -- and: 12 a b c
        --   stores into <a> the bitwise and of <b> and <c>
        12 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, b .&. c)], pc = pc + 4}, [])
        -- or: 13 a b c
        --   stores into <a> the bitwise or of <b> and <c>
        13 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
                c = v (mem ! (pc + 3))
            in Just (m{reg = reg // [(a, b .|. c)], pc = pc + 4}, [])
        -- not: 14 a b
        --   stores 15-bit bitwise inverse of <b> in <a>
        14 ->
            let a = r (mem ! (pc + 1))
                b = v (mem ! (pc + 2))
            in debug (intercalate " " ["not", show a, show b]) $ Just (m{reg = reg // [(a, complement b .&. 32767)], pc = pc + 3}, [])
        -- rmem: 15 a b
        --   read memory at address <b> and write it to <a>
        15 ->
            let a = (mem ! (pc + 1))
                b = (mem ! (pc + 2))
            -- in debug (intercalate " " ["rmem", show a, show b]) $ Just (m{reg = reg // [(r a, mem ! (v b))], pc = pc + 3}, [])
            --
            in debug (intercalate " " ["rmem", show a, show b]) $ case (x (mem ! (pc + 1)), v (mem ! (pc + 2))) of
                (Reg a, b) -> Just (m{reg = reg // [(a, mem ! b)], pc = pc + 3}, [])
        -- wmem: 16 a b
        --   write the value from <b> into memory at address <a>
        16 ->
            let a = (mem ! (pc + 1))
                b = (mem ! (pc + 2))
            -- in debug (intercalate " " ["wmem", show a, show b]) $ Just (m{reg = reg // [(r a, mem ! b)], pc = pc + 3}, [])
            in debug (intercalate " " ["wmem", show a, show b]) $
                case (x (v (mem ! (pc + 1))), v (mem ! (pc + 2))) of
                    (Reg a, b) -> Just (m{reg = reg // [(a, b)], pc = pc + 3}, [])
                    (Lit a, b) -> Just (m{mem = mem // [(a, b)], pc = pc + 3}, [])
        -- call: 17 a
        --   write the address of the next instruction to the stack and jump to <a>
        17 ->
            let a = v (mem ! (pc + 1))
            in debug "call" $ Just (m{stack = (pc + 2):stack, pc = a}, [])
        -- ret: 18
        --   remove the top element from the stack and jump to it; empty stack = halt
        18 ->
            case stack of
                [] -> Nothing
                a:ss -> Just (m{stack = ss, pc = a}, [])
        -- out: 19 a
        --   write the character represented by ascii code <a> to the terminal
        19 ->
            let a = v (mem ! (pc + 1))
            in Just (m{pc = pc + 2}, [chr (fromIntegral a)])
        20 -> error $ "in not implemented"
        21 -> Just (m{pc = pc + 1}, [])
        x  -> error $ "unknown op: " ++ show x ++ " at pc: " ++ show pc
    where
        -- Literal value or from register
        x :: Word16 -> Val
        x a | a < 32768 = Lit a
            | a < 32776 = Reg (a - 32768)
            | otherwise = error "numbers 32776..65535 are invalid"

        -- Register offset
        r :: Word16 -> Word16
        r (x -> Reg a) = a

        -- Literal value or value from register
        v (x -> Lit a) = a
        v (x -> Reg a) = reg ! a

        rmem a = mem ! a

        debug :: String -> a -> a
        debug string expr
            | debugFlag = trace string expr
            | otherwise = expr

        debugShow :: Show a => a -> b -> b
        debugShow = debug . show

readFile :: FilePath -> IO (Array Word16 Word16)
readFile filePath = do
    buf <- L.readFile filePath
    let len = L.length buf
        n = fromIntegral $ len `div` 2
    return $ listArray (0, fromIntegral n - 1) $ runGet (replicateM n getWord16le) buf

run debugFlag m = case step debugFlag m of
            Nothing -> return m
            Just (m', s) -> do putStr s
                               run debugFlag m'

main = do
    mem <- readFile "challenge.bin"
    -- let mem = testinput
    let reg = listArray (0, 7) (replicate 8 0)
        stack = []
        pc = 0
    let m = Machine{..}
    run True m
