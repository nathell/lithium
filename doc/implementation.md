# Overview

To be written...

# ABI

## Registers

In Lithium-generated code, the following registers have special meaning and should not be clobbered by primitives:

- AX - holds the result of evaluation of an expression. The basic premise of Lithium is "compile each expression to code that evaluates it and puts its result in AX".
- SP - stack pointer. In general not manipulated directly, except when allocating space for temporary storage in the prolog of a function or cleaning up after a function call.
- BP - frame pointer. Remains constant throughout a function call.
- SI - top of the heap.
- DI - pointer to the closure currently being called.
- CS, DS, ES, SS - Lithium never manipulates the segment registers as it assumes a flat memory model.

These registers are general-purpose and can be used by primitives:

- BX - used temporarily while calling closures and evaluating `recur` expressions.
- CX - used only by the `put-pixel` primitive.
- DX - used only by the `quot` and `rem` primitives.

## Memory

Lithium assumes a flat memory model. As it currently produces 16-bit binaries, this means the 'tiny' memory model, where code and data both live in the same 64KB segment.

At any given point in the program's execution, the layout of that segment looks roughly like this:

       +-----------------+ origin (0x100)
       |  compiled code  |
       +-----------------+
       |       heap      |
       +-----------------+ top of the heap (SI)
       |      ...        |
       |    free space   |
       |      ...        |
       +-----------------+ tip of stack (SP)
       |    temporary    |
       |     storage     |
     ^-+-----------------+ frame pointer (BP)
     | | return address  |
     F +-----------------+
     R |    function     |
     A |    arguments    |
     M +-----------------+
     E |     previous    |
     | | closure pointer |
     v-+-----------------+ end of first frame
       |     frame 2     |
       +-----------------+
       |       ...       |
       +-----------------+
       |     frame n     |
       +-----------------+ bottom of stack
       |      global     |
       |   definitions   |
       +-----------------+ 0xFFFE
