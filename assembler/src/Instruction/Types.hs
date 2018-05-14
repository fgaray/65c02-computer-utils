{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Instruction.Types where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Text

data Register =
      A
    | X
    | Y

data Memory = Memory Int


-- FROM https://patpend.net/technical/6502/6502ref.html#InstructionSet
data Instruction next where
    -- Load & Store Instructions
    Lda :: next -> Instruction next -- load accumulator    NZ
    Ldx :: next -> Instruction next -- load X index    NZ
    Ldy :: next -> Instruction next -- load Y index    NZ
    Sta :: next -> Instruction next -- store accumulator   -
    Stx :: next -> Instruction next -- store X index   -
    Sty :: next -> Instruction next -- store Y index   -
    Stz :: next -> Instruction next -- store zero  -
    -- Stack Operations
    Pha :: next -> Instruction next -- push accumulator    -
    Phx :: next -> Instruction next -- push X index    -
    Phy :: next -> Instruction next -- push Y index    -
    Php :: next -> Instruction next -- push processor flags    -
    Pla :: next -> Instruction next -- pull (pop) accumulator  NZ
    Plx :: next -> Instruction next -- pull (pop) X index  NZ
    Ply :: next -> Instruction next -- pull (pop) Y index  NZ
    Plp :: next -> Instruction next -- pull (pop) processor flags  All
    Tsx :: next -> Instruction next -- transfer stack pointer to X NZ
    Txs :: next -> Instruction next -- transfer stack pointer to X -
    -- Increment & Decrement Operations
    Ina :: next -> Instruction next -- increment accumulator
    Inx :: next -> Instruction next -- increment X index
    Iny :: next -> Instruction next -- increment Y index
    Dea :: next -> Instruction next -- decrement accumulator
    Dex :: next -> Instruction next -- decrement X index
    Dey :: next -> Instruction next -- decrement Y index
    Inc :: Memory -> next -> Instruction next -- increment memory location
    Dec :: Memory -> next -> Instruction next -- decrement memory location
    -- Shift Operations
    Asl :: next -> Instruction next -- arithmetic shift left, high bit into carry  NZC
    Lsr :: next -> Instruction next -- logical shift right, low bit into carry N=0 ZC
    Rol :: next -> Instruction next -- rotate left through carry   NZC
    Ror :: next -> Instruction next -- rotate right through carry  NZC
    -- Logical Operations
    AND :: next -> Instruction next -- and accumulator NZ
    ORA :: next -> Instruction next -- or accumulator  NZ
    EOR :: next -> Instruction next -- exclusive-or accumulator    NZ
    BIT :: next -> Instruction next -- test bits against accumulator (1)   N=M7 V=M6 Z
    CMP :: next -> Instruction next -- compare with accumulator    NZC
    CPX :: next -> Instruction next -- compare with X index    NZC
    CPY :: next -> Instruction next -- compare with Y index    NZC
    TRB :: next -> Instruction next -- test and reset bits x
    TSB :: next -> Instruction next -- test and set bits   x
    RMB :: next -> Instruction next -- reset memory bit    x
    SMB :: next -> Instruction next -- reset memory bit    x
    -- Math Operations
    ADC :: next -> Instruction next -- add accumulator, with carry NZCV
    SBC :: next -> Instruction next -- subtract accumulator, with borrow   NZCV
    -- Flow Control Instructions
    JMP :: next -> Instruction next -- unconditional jump  -
    JSR :: next -> Instruction next -- jump Subroutine -
    RTS :: next -> Instruction next -- return from Subroutine  -
    RTI :: next -> Instruction next -- return from Interrupt   From Stack
    BRA :: next -> Instruction next -- branch Always   -
    BEQ :: next -> Instruction next -- branch on equal (zero set)  -
    BNE :: next -> Instruction next -- branch on not equal (zero clear)    -
    BCC :: next -> Instruction next -- branch on carry clear (2)   -
    BCS :: next -> Instruction next -- branch on carry set (2) -
    BVC :: next -> Instruction next -- branch on overflow clear    -
    BVS :: next -> Instruction next -- branch on overflow set  -
    BMI :: next -> Instruction next -- branch on minus -
    BPL :: next -> Instruction next -- branch on plus  -
    BBR :: next -> Instruction next -- branch on bit reset (zero)  -
    BBS :: next -> Instruction next -- branch on bit set (one) -
    -- Processor Status Instructions
    CLC :: next -> Instruction next -- clear carry flag    C=0
    CLD :: next -> Instruction next -- clear decimal mode  D=0
    CLI :: next -> Instruction next -- clear interrupt disable bit I=0
    CLV :: next -> Instruction next -- clear overflow flag V=0
    SEC :: next -> Instruction next -- set carry flag  C=1
    SED :: next -> Instruction next -- set decimal mode    D=1
    SEI :: next -> Instruction next -- set interrupt disable bit   I=1
    -- Transfer Instructions
    TAX :: next -> Instruction next -- transfer accumulator to X index NZ
    TAY :: next -> Instruction next -- transfer accumulator to Y index NZ
    TXA :: next -> Instruction next -- transfer X index to accumulator NZ
    TYA :: next -> Instruction next -- transfer Y index to accumulator NZ
    -- Misc Instructions
    Nop :: next -> Instruction next
    BRK :: next -> Instruction next -- force break B=1

    EndInstructionMonad :: Instruction next
    deriving (Functor)

type InstructionMonad = Free Instruction

$(makeFreeCon 'Nop)
$(makeFreeCon 'Lda)
$(makeFreeCon 'Ldx)
$(makeFreeCon 'Ldy)
$(makeFreeCon 'Sta)
$(makeFreeCon 'Stx)
$(makeFreeCon 'Sty)
$(makeFreeCon 'Stz)
$(makeFreeCon 'Pha)
$(makeFreeCon 'Phx)
$(makeFreeCon 'Phy)
$(makeFreeCon 'Php)
$(makeFreeCon 'Pla)
$(makeFreeCon 'Plx)
$(makeFreeCon 'Ply)
$(makeFreeCon 'Plp)
$(makeFreeCon 'Tsx)
$(makeFreeCon 'Txs)
$(makeFreeCon 'Ina)
$(makeFreeCon 'Inx)
$(makeFreeCon 'Iny)
$(makeFreeCon 'Dea)
$(makeFreeCon 'Dex)
$(makeFreeCon 'Dey)
$(makeFreeCon 'Inc)
$(makeFreeCon 'Dec)
$(makeFreeCon 'Asl)
$(makeFreeCon 'Lsr)
$(makeFreeCon 'Rol)
$(makeFreeCon 'Ror)
$(makeFreeCon 'AND)
$(makeFreeCon 'ORA)
$(makeFreeCon 'EOR)
$(makeFreeCon 'BIT)
$(makeFreeCon 'CMP)
$(makeFreeCon 'CPX)
$(makeFreeCon 'CPY)
$(makeFreeCon 'TRB)
$(makeFreeCon 'TSB)
$(makeFreeCon 'RMB)
$(makeFreeCon 'SMB)
$(makeFreeCon 'ADC)
$(makeFreeCon 'SBC)
$(makeFreeCon 'JMP)
$(makeFreeCon 'JSR)
$(makeFreeCon 'RTS)
$(makeFreeCon 'RTI)
$(makeFreeCon 'BRA)
$(makeFreeCon 'BEQ)
$(makeFreeCon 'BNE)
$(makeFreeCon 'BCC)
$(makeFreeCon 'BCS)
$(makeFreeCon 'BVC)
$(makeFreeCon 'BVS)
$(makeFreeCon 'BMI)
$(makeFreeCon 'BPL)
$(makeFreeCon 'BBR)
$(makeFreeCon 'BBS)
$(makeFreeCon 'CLC)
$(makeFreeCon 'CLD)
$(makeFreeCon 'CLI)
$(makeFreeCon 'CLV)
$(makeFreeCon 'SEC)
$(makeFreeCon 'SED)
$(makeFreeCon 'SEI)
$(makeFreeCon 'TAX)
$(makeFreeCon 'TAY)
$(makeFreeCon 'TXA)
$(makeFreeCon 'TYA)
$(makeFreeCon 'EndInstructionMonad)
