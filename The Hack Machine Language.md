# The Hack Machine Language

A machine language is an abstract set of instructions that manipulates registers in a memory by means of a processing unit.

Hack is a 16-bit computer that adheres to the Von Neuman architecture, and therefore contains the following units
1. Memory

Hack Has two types of memory units
- Data Memory (RAM): stores the data the program manipulates
- Instruction Memory (ROM): stores the program instructions

both have a 15-bit address space (32KB memory)

Note: there will always be an address selected at any given time, since the memory's address will always contain a value. These selected registers are called M register for RAM and instruction register for ROM. 

The RAM and ROM address are selected based on the value of the A register.

2. Registers

Hack instructions interact with 3 types of 16-bit registers
- Data Register (D)
- Address register (A)
- Selected Data Memory Register (M)

the @xxx syntax is the only way to introduce constants into the system

# The Hack Language Specification
## The A-Instruction
In essence, the A-instruction sets the A register to a 15-bit value. It has the following syntax

```
Binary: 0vvvvvvvvvvvvvvv where v=[0|1]
Symbolic: @xxx where xxx=[0...32767]
```

The op-code for an A-instruction is 0.

The Hack instruction @xxx does the following:
- sets the A register to the value of xxx, which sets the stage for the subsequent C instruction to either manipulate the RAM (by setting the M regsiter to A) or performing a jump (by setting the instruction register to A)

## The C-Instruction

The C instruction has the following format
- an op-code of 1
- two unused bits (set to 1 by convention)
- 7 bits that represent the `comp` field 
- 3 bits that represent the `dest` field
- 3 bits that represent the `jump` field

```
Binary: 111accccccdddjjj
Symbolic: dest = comp;jump
```

The C instruction performs three classes of operations:
1. Compute a Value

Depending on the value of `acccccc`, up to 28 listed computations can be performed. (see figure 4.5)

A computation is fed to the ALU and therefre is read from two registers. These registers are
- The D register
- if a == 0 then A else M

2. Store a computed value

Depending on the value of `ddd`, the ALU can store its output from zero up to three possible destinations simultaneously.

if the first `d` == 1 then store computed value in A
if the second `d` == 1 then store the computed value in D
if the third `d` == 1 then store the computed value in M

3. Perform a jump instruction

Figure 4.5 has 8 different jump instruction based on the value of `jjj` (which depend on the ALU output)

the default behaviour when a cycle ends is to fetch the next instruction (`jjj` = 000). If a jump is performed, the address of the next instruction is read from the A register.

for unconditional jump, the value for `comp` is zero (an ALU output)

It is a good practice that any C instructions thet reference M should no specify any jumps (and vice versa)

## Symbols
### Predefined symbols
### Label Symbols
### Variable Symbols

## IO Handling