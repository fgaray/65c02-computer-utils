module Processor.Types where

import Data.Int

data Status = Status
    { n   :: Bool -- Negative result
    , v   :: Bool -- Sign bit overflow
    , one :: Bool -- Undefined, always set to 1
    , b   :: Bool -- Break flag (set by BRK instruction)
    , d   :: Bool -- Decimal mode enabled
    , i   :: Bool -- IRQ disabled
    , z   :: Bool -- Zero result
    , c   :: Bool -- Arithmetic carry (borrow)
    }

data Registers = Registers
    { a      :: Int8
    , x      :: Int8
    , y      :: Int8
    , sp     :: Int8
    , pc     :: Int16
    } 


data State = State
    { registers :: Registers
    , status    :: Status
    }
