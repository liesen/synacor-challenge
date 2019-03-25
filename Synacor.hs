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
import Debug.Trace


data Machine = Machine
    { mem :: L.ByteString
    , reg :: Array Word16 Word16
    , stack :: [Word16]
    , pc :: Word16
    }

instance Show Machine where
    show Machine{..} = concat ["[", show pc, ",", show reg, ",", show (take 3 stack), "]"]

data Op 
    = Halt
    | Set Word16 Word16
    | Push Word16
    | Pop Word16
    | Eq Word16 Word16 Word16
    | Gt Word16 Word16 Word16
    | Jmp Word16
    | Jt Word16 Word16
    | Jf Word16 Word16
    | Add Word16 Word16 Word16
    | Mult Word16 Word16 Word16
    | Mod Word16 Word16 Word16
    | And Word16 Word16 Word16
    | Or Word16 Word16 Word16
    | Not Word16 Word16
    | Rmem Word16 Word16
    | Wmem Word16 Word16
    | Call Word16
    | Ret
    | Out Word16
    | In Word16
    | Noop
  deriving (Show, Eq)

instance Binary Op where
    put = error "undefined"
    get = getWord16le >>= \case
            0 -> pure Halt
            1 -> Set <$> g <*> g
            2 -> Push <$> g
            3 -> Pop <$> g
            4 -> Eq <$> g <*> g <*> g
            5 -> Gt <$> g <*> g <*> g
            6 -> Jmp <$> g
            7 -> Jt <$> g <*> g
            8 -> Jf <$> g <*> g
            9 -> Add <$> g <*> g <*> g
            10 -> Mult <$> g <*> g <*> g
            11 -> Mod <$> g <*> g <*> g
            12 -> And <$> g <*> g <*> g
            13 -> Or <$> g <*> g <*> g
            14 -> Not <$> g <*> g
            15 -> Rmem <$> g <*> g
            16 -> Wmem <$> g <*> g
            17 -> Call <$> g
            18 -> pure Ret
            19 -> Out <$> g
            20 -> In <$> g
            21 -> pure Noop
            x -> fail ("unknown op: " ++ show x)
        where
            g = getWord16le

-- Test input
testinput = runPut $ mapM_ putWord16le [9,32768,32769,4,19,32768,0]

data Val = Lit Word16
         | Reg Word16

step :: Bool -> Machine -> Maybe (Machine, [Char])
step debugFlag m@Machine{..} = 
    -- let op = ops ! fromIntegral pc
    --     sz = size op
    let op = runGet get (L.drop (fromIntegral pc) mem)
        sz = size op
    in debugShow (m, op) $ case op of
        -- halt: 0
        --   stop execution and terminate the program
        Halt -> Nothing
        -- set: 1 a b
        --   set register <a> to the value of <b>
        Set (r -> a) (v -> b) ->
            debug (concat ["set r", show a, " = ", show b]) $
                Just (m{reg = reg // [(a, b)], pc = pc + sz}, [])
        -- push: 2 a
        --   push <a> onto the stack
        Push (v -> a) -> Just (m{stack = a:stack, pc = pc + sz}, [])
        -- pop: 3 a
        --   remove the top element from the stack and write it into <a>; empty stack = error
        Pop (r -> a) -> Just (m{stack = tail stack, reg = reg // [(a, head stack)], pc = pc + sz}, [])
        -- eq: 4 a b c
        --   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
        Eq (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, if b == c then 1 else 0)], pc = pc + sz}, [])
        -- gt: 5 a b c
        --   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
        Gt (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, if b > c then 1 else 0)], pc = pc + sz}, [])
        -- jmp: 6 a
        --   jump to <a>
        Jmp (v -> a) -> Just (m{pc = a * 2}, [])
        -- jt: 7 a b
        --   if <a> is nonzero, jump to <b>
        Jt (v -> 0) _ -> Just (m{pc = pc + sz}, [])
        Jt (v -> a) (v -> b) -> Just (m{pc = b * 2}, [])
        -- jf: 8 a b
        --   if <a> is zero, jump to <b>
        Jf (v -> 0) (v -> b) ->
            debug (concat ["<a> is zero, jumping to ", show b]) $
                Just (m{pc = b * 2}, [])
        Jf (v -> a) _ -> Just (m{pc = pc + sz}, [])
        -- add: 9 a b c
        --   assign into <a> the sum of <b> and <c> (modulo 32768)
        Add (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, (b + c) `mod` 32768)], pc = pc + sz}, [])
        -- mult: 10 a b c
        --   store into <a> the product of <b> and <c> (modulo 32768)
        Mult (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, (b * c) `mod` 32768)], pc = pc + sz}, [])
        -- mod: 11 a b c
        --   store into <a> the remainder of <b> divided by <c>
        Mod (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, b `mod` c)], pc = pc + sz}, [])
        -- and: 12 a b c
        --   stores into <a> the bitwise and of <b> and <c>
        And (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, b .&. c)], pc = pc + sz}, [])
        -- or: 13 a b c
        --   stores into <a> the bitwise or of <b> and <c>
        Or (r -> a) (v -> b) (v -> c) ->
            Just (m{reg = reg // [(a, b .|. c)], pc = pc + sz}, [])
        -- not: 14 a b
        --   stores 15-bit bitwise inverse of <b> in <a>
        Not (r -> a) (v -> b) ->
            Just (m{reg = reg // [(a, complement b .&. 32767)], pc = pc + sz}, [])
        -- rmem: 15 a b
        --   read memory at address <b> and write it to <a>
        Rmem (x -> Reg a) (x -> Lit b) ->
            let b' = rmem b
            in debug ("rmem(r:" ++ show a ++ "=" ++ show (reg ! a) ++ ",@" ++ show b ++ "=" ++ show b' ++ ")") $
                Just (m{reg = reg // [(a, b')], pc = pc + sz}, [])
        Rmem (x -> Reg a) (x -> Reg b) ->
            let b' = rmem (reg ! b)
            in debug ("rmem(r:" ++ show a ++ "=" ++ show (reg ! a) ++ ",@" ++ show b ++ "=" ++ show b' ++ ")") $
                Just (m{reg = reg // [(a, b')], pc = pc + sz}, [])
        -- wmem: 16 a b
        --   write the value from <b> into memory at address <a>
        Wmem (x -> Reg a) (x -> Lit b) ->
            let b' = b
            in debug ("wmem(r:" ++ show a ++ "=" ++ show (reg ! a) ++ ",@" ++ show b ++ "=" ++ show b' ++ ")") $
                Just (m{reg = reg // [(a, b')], pc = pc + sz}, [])
        -- call: 17 a
        --   write the address of the next instruction to the stack and jump to <a>
        Call (v -> a) ->
            debug "call" $ Just (m{stack = (pc + sz) `div` 2:stack, pc = a * 2}, [])
        Ret -> error $ show op ++ " not implemented"
        Out (chr . fromIntegral . v -> a) ->
            Just (m{pc = pc + sz}, [a])
        In a -> error $ show op ++ " not implemented"
        Noop -> Just (m{pc = pc + sz}, [])
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

        rmem a = runGet getWord16le (L.drop (fromIntegral a * 2) mem)

        debug :: String -> a -> a
        debug string expr
            | debugFlag = trace string expr
            | otherwise = expr

        debugShow :: Show a => a -> b -> b
        debugShow = debug . show

run debugFlag m = case step debugFlag m of
            Nothing -> return m
            Just (m', s) -> do putStr s
                               run debugFlag m'

-- Size in bytes of an op
size = \case
    Halt -> 2
    Set _ _ -> 6
    Push _ -> 4
    Pop _ -> 4
    Eq _ _ _ -> 8
    Gt _ _ _ -> 8
    Jmp _ -> 4
    Jt _ _ -> 6
    Jf _ _ -> 6
    Add _ _ _ -> 8
    Mult _ _ _ -> 8
    Mod _ _ _ -> 8
    And _ _ _ -> 8
    Or _ _ _ -> 8
    Not _ _ -> 6
    Rmem _ _ -> 6
    Wmem _ _ -> 6
    Call _ -> 4
    Ret -> 2
    Out _ -> 4
    In _ -> 4
    Noop -> 2

main = do
    mem <- L.readFile "challenge.bin"
    -- let mem = testinput
    let reg = listArray (0, 7) (replicate 8 0)
        stack = []
        pc = 0
    let m = Machine{..}
    print m
    print $ step True m
    run True m
