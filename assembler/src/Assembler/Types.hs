module Assembler.Types where

import Text.Printf

-- addressing mode 
data Mode =
      Implicit
    | Accumulator
    deriving (Show)

data Flag =
      N
    | Z
    deriving (Show)

data Opcode = Opcode
    { op     :: Int    -- the opcode
    , length :: Int    -- the length of the instruction in bytes
    , cyc    :: Int    -- the number of cycles the instruction takes
    , mode   :: Mode   -- the addressing mode of the instruction
    , flags  :: [Flag] -- the flags affected by the instruction
    }

instance Show Opcode where
    show (Opcode op length cyc mode flags) =
        printf "%x %d %d %s %s" op length cyc (show mode) (show flags)
