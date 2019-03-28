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
    , debugFlag :: Bool
    }

instance Show Machine where
    show Machine{..} = concat ["[", show pc, ",", show reg, ",", show (take 3 stack), "]"]

data Op
    = Halt
    | Set
    | Push
    | Pop
    | Eq
    | Gt
    | Jmp
    | Jt
    | Jf
    | Add
    | Mult
    | Mod
    | And
    | Or
    | Not
    | Rmem
    | Wmem
    | Call
    | Ret
    | Out
    | In
    | Noop
  deriving (Show, Eq, Enum)

-- Test input
testinput = runPut $ mapM_ putWord16le [9,32768,32769,4,19,32768,0]

data Val = Lit Word16
         | Reg Word16

instance Show Val where
    show (Lit x) = "!" ++ show x
    show (Reg x) = "@" ++ show x

step :: Machine -> IO (Maybe Machine)
step m@Machine{..} =
    debugShow m $ case op of
        -- halt: 0
        --   stop execution and terminate the program
        Halt -> return $ debug "halt" Nothing
        -- set: 1 a b
        --   set register <a> to the value of <b>
        Set ->
            let [a, b] = take 2 args
            in return $ debug (intercalate " " ["set", show a, show b]) $
                Just (m{reg = reg // [(r a, v b)], pc = pc + 3})
        -- push: 2 a
        --   push <a> onto the stack
        Push ->
            let [a] = take 1 args
            in return $ debug (intercalate " " ["push", show a]) $ Just (m{stack = v a:stack, pc = pc + 2})
        -- pop: 3 a
        --   remove the top element from the stack and write it into <a>; empty stack = error
        Pop -> do
            let [a] = take 1 args
                s:ss = stack
            debugOp [a] $ return $ Just (m{stack = ss, reg = reg // [(r a, s)], pc = pc + 2})
        -- eq: 4 a b c
        --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        Eq -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, if v b == v c then 1 else 0)], pc = pc + 4})
        -- gt: 5 a b c
        --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        Gt -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, if v b > v c then 1 else 0)], pc = pc + 4})
        -- jmp: 6 a
        --   jump to <a>
        Jmp -> do
            let [a] = take 1 args
            debugOp [a] $ return $ Just (m{pc = v a})
        -- jt: 7 a b
        --   if <a> is nonzero, jump to <b>
        Jt -> do
            let [a, b] = take 2 args
            debugOp [a, b] $ return $ Just (m{pc = if v a /= 0 then v b else pc + 3})
        -- jf: 8 a b
        --   if <a> is zero, jump to <b>
        Jf -> do
            let [a, b] = take 2 args
            debugOp [a, b] $ return $ Just (m{pc = if v a == 0 then v b else pc + 3})
        -- add: 9 a b c
        --   assign into <a> the sum of <b> and <c> (modulo 32768)
        Add -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, (v b + v c) `mod` 32768)], pc = pc + 4})
        -- mult: 10 a b c
        --   store into <a> the product of <b> and <c> (modulo 32768)
        Mult -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, (v b * v c) `mod` 32768)], pc = pc + 4})
        -- mod: 11 a b c
        --   store into <a> the remainder of <b> divided by <c>
        Mod -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, v b `mod` v c)], pc = pc + 4})
        -- and: 12 a b c
        --   stores into <a> the bitwise and of <b> and <c>
        And -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, v b .&. v c)], pc = pc + 4})
        -- or: 13 a b c
        --   stores into <a> the bitwise or of <b> and <c>
        Or -> do
            let [a, b, c] = take 3 args
            debugOp [a, b, c] $
                return $ Just (m{reg = reg // [(r a, v b .|. v c)], pc = pc + 4})
        -- not: 14 a b
        --   stores 15-bit bitwise inverse of <b> in <a>
        Not -> do
            let [a, b] = take 2 args
            debugOp [a, b] $
                return $ Just (m{reg = reg // [(r a, complement (v b) .&. 32767)], pc = pc + 3})
        -- rmem: 15 a b
        --   read memory at address <b> and write it to <a>
        Rmem -> do
            let [a, b] = take 2 args
            debugOp [a, b] $
                return $ case (x a, v b) of
                    (Reg a, b) -> Just (m{reg = reg // [(a, mem ! b)], pc = pc + 3})
        -- wmem: 16 a b
        --   write the value from <b> into memory at address <a>
        Wmem -> do
            let [a, b] = take 2 args
            debugOp [a, b] $
                -- return $ Just $ (write (v a) (v b)){pc = pc + 3}
                return $ case (x (v a), v b) of
                    (Reg a, b) -> Just (m{reg = reg // [(a, b)], pc = pc + 3})
                    (Lit a, b) -> Just (m{mem = mem // [(a, b)], pc = pc + 3})
        -- call: 17 a
        --   write the address of the next instruction to the stack and jump to <a>
        Call -> do
            let [a] = take 1 args
            debugOp [a] $ return $ Just (m{stack = (pc + 2):stack, pc = v a})
        -- ret: 18
        --   remove the top element from the stack and jump to it; empty stack = halt
        Ret -> debugOp [] $ return $ case stack of
                [] -> Nothing
                a:ss -> Just (m{stack = ss, pc = a})
        -- out: 19 a
        --   write the character represented by ascii code <a> to the terminal
        Out -> do
            let [a] = take 1 args
            putChar $ chr (fromIntegral (v a))
            debugOp [a] $ return $ Just (m{pc = pc + 2})
        -- in: 20 a
        --   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
        In -> do
            let [a] = take 1 args
            b <- getChar
            debugOp [a] $ return $
                case x a of
                    Reg a -> Just (m{reg = reg // [(a, fromIntegral (ord b))], pc = pc + 2})
        Noop -> debugOp [] $ return $ Just (m{pc = pc + 1})
    where
        opcode = mem ! pc
        op = toEnum (fromIntegral opcode)
        args = map (mem !) [pc + 1..]

        -- Literal value or register
        x :: Word16 -> Val
        x a | a < 32768 = Lit a
            | a < 32776 = Reg (a - 32768)
            | otherwise = error "numbers 32776..65535 are invalid"

        -- Register index
        r :: Word16 -> Word16
        r (x -> Reg a) = a

        -- Literal value or value from register
        v (x -> Lit a) = a
        v (x -> Reg a) = reg ! a

        write a b
            | a < 32768 = m{mem = mem // [(a, b)]}
            | a < 32776 = m{reg = reg // [(a - 32768, b)]}
            | otherwise = error "numbers 32776..65535 are invalid"

        read a | a < 32768 = mem ! a
               | a < 32776 = mem ! (reg ! (a - 32768))
               | otherwise = error "numbers 32776..65535 are invalid"

        debug :: String -> a -> a
        debug string expr
            | debugFlag = trace string expr
            | otherwise = expr

        debugShow :: Show a => a -> b -> b
        debugShow = debug . show

        debugOp :: [Word16] -> a -> a
        debugOp args = debug $ intercalate " " (map toLower (show op):map show args)

readFile :: FilePath -> IO (Array Word16 Word16)
readFile filePath = do
    buf <- L.readFile filePath
    let len = L.length buf
        n = fromIntegral $ len `div` 2
    return $ listArray (0, fromIntegral n - 1) $ runGet (replicateM n getWord16le) buf

run m = step m >>= \case
            Nothing -> return m
            Just m' -> run m'

main = do
    -- raw <- L.readFile "challenge.bin"
    mem <- readFile "challenge.bin"
    let reg = listArray (0, 7) (replicate 8 0)
        stack = []
        pc = 0
        debugFlag = False
    let m = Machine{..}
    run m
