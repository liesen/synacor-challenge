{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Data.Array
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Debug.Trace


data Machine = Machine
    { mem :: L.ByteString
    , reg :: Array Int Word16
    , stack :: [Word16]
    , pc :: Int
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
    | Ret Word16
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
            18 -> Ret <$> g
            19 -> Out <$> g
            20 -> In <$> g
            21 -> pure Noop
            x -> error $ "unknown op: " ++ show x
        where
            g = getWord16le

a = runPut $ mapM_ putWord16le [9,32768,32769,4,19,32768]

step :: Machine -> Machine
step m@Machine{..} = 
    let op = runGet get (L.drop (fromIntegral pc) mem)
        sz = size op in
    traceShow op $ case op of
        Halt -> error "not implemented"
        Set a b -> error "not implemented"
        Push a -> error "not implemented"
        Pop a -> error "not implemented"
        Eq a b c -> error "not implemented"
        Gt a b c -> error "not implemented"
        Jmp a -> error "not implemented"
        Jt a b -> error "not implemented"
        Jf a b -> error "not implemented"
        Add a b c -> error "not implemented"
        Mult a b c -> error "not implemented"
        Mod a b c -> error "not implemented"
        And a b c -> error "not implemented"
        Or a b c -> error "not implemented"
        Not a b -> error "not implemented"
        Rmem a b -> error "not implemented"
        Wmem a b -> error "not implemented"
        Call a -> error "not implemented"
        Ret a -> error "not implemented"
        Out a -> error "not implemented"
        In a -> error "not implemented"
        Noop -> m {pc = pc + sz}
    where
        lookup v | v < 32768 = v
                 | v < 32776 = reg ! (fromIntegral v - 32768)
                 | otherwise = error "numbers 32776..65535 are invalid"

-- Size of an op
size = \case
    Halt -> 1
    Set _ _ -> 3
    Push _ -> 2
    Pop _ -> 2
    Eq _ _ _ -> 4
    Gt _ _ _ -> 4
    Jmp _ -> 2
    Jt _ _ -> 3
    Jf _ _ -> 3
    Add _ _ _ -> 4
    Mult _ _ _ -> 4
    Mod _ _ _ -> 4
    And _ _ _ -> 4
    Or _ _ _ -> 4
    Not _ _ -> 3
    Rmem _ _ -> 3
    Wmem _ _ -> 3
    Call _ -> 2
    Ret _ -> 2
    Out _ -> 2
    In _ -> 2
    Noop -> 1

main = do
    mem <- L.readFile "challenge.bin"
    let mem = b
    let reg = listArray (0, 7) (replicate 8 0)
        stack = []
        pc = 0
    let m = Machine{..}
    print m
    print $ step m
    print $ step $ step m
    print $ step $ step $ step m