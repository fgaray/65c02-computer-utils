module Assembler.Generator where

import Instruction.Types
import Assembler.Types
import Control.Monad.Free



generateOpcode :: InstructionMonad next -> [Opcode]
generateOpcode (Free EndInstructionMonad) = []
generateOpcode (Free (Nop next))          = Opcode 0xea 1 2 Implicit [] : generateOpcode next
generateOpcode (Free (Inx next))          = Opcode 0xe8 1 2 Accumulator [N, Z] : generateOpcode next
generateOpcode (Free (Iny next))          = Opcode 0xc8 1 2 Accumulator [N, Z] : generateOpcode next
generateOpcode (Free (Dex next))          = Opcode 0xca 1 2 Accumulator [N, Z] : generateOpcode next
generateOpcode (Free (Dey next))          = Opcode 0x88 1 2 Accumulator [N, Z] : generateOpcode next
