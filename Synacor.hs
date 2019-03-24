{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
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
    show Machine{..} = concat ["[", show reg, ",", show pc, "]"]

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

step :: Bool -> Machine -> Maybe (Machine, [Char])
step debugFlag m@Machine{..} = 
    -- let op = ops ! fromIntegral pc
    --     sz = size op
    let op = runGet get (L.drop (fromIntegral pc) mem)
        sz = size op
    in debugShow (m, op) $ case op of
        Halt -> Nothing
        Set (lookup -> a) (lookup -> b) ->
            Just (m{reg = reg // [(fromIntegral a, b)], pc = pc + sz}, [])
        Push a -> error $ show op ++ " not implemented"
        Pop a -> error $ show op ++ " not implemented"
        Eq a b c -> error $ show op ++ " not implemented"
        Gt a b c -> error $ show op ++ " not implemented"
        Jmp (lookup -> a) ->
            Just (m{pc = a * 2}, [])
        Jt a b -> error $ show op ++ " not implemented"
        Jf a b -> error $ show op ++ " not implemented"
        Add (lookup -> a) (lookup -> b) (lookup -> c) ->
            Just (m{reg = reg // [(a, (b + c) `mod` 32768)], pc = pc + sz}, [])
        Mult a b c -> error $ show op ++ " not implemented"
        Mod a b c -> error $ show op ++ " not implemented"
        And a b c -> error $ show op ++ " not implemented"
        Or a b c -> error $ show op ++ " not implemented"
        Not a b -> error $ show op ++ " not implemented"
        Rmem a b -> error $ show op ++ " not implemented"
        Wmem a b -> error $ show op ++ " not implemented"
        Call (lookup -> a) ->
            Just (m{stack = (pc + sz):stack, pc = a}, [])
        Ret -> error $ show op ++ " not implemented"
        Out (chr . fromIntegral . lookup -> a) ->
            Just (m{pc = pc + sz}, [a])
        In a -> error $ show op ++ " not implemented"
        Noop -> Just (m{pc = pc + sz}, [])
    where
        lookup v | v < 32768 = v
                 | v < 32776 = reg ! (fromIntegral v - 32768)
                 | otherwise = error "numbers 32776..65535 are invalid"

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
