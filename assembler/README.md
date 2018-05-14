# 65C02 Assembler


This is an early stage of a 65C02 assembler in Haskell. See the TODO for the
things that are still missing.

The objective of this assembler is to be use in a future 8-bit multitasking
computer using this processor. I have not decide yet if it is going to be a
cooperative multitasking OS or preemptive (that would require additional
hardware).

The testing is going to be against [xa](http://www.floodgap.com/retrotech/xa/).
At last it need to generate the same opcodes.

# TODO/Done

* [X] Instruction monad.
* [ ] Parser of assembly code
* [ ] Opcode generation.
* [ ] Binary generation.
* [ ] Global memory.
* [ ] Optimizations to the generated binary.
