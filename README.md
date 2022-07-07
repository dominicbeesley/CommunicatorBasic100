# CommunicatorBasic100
Disassembly of Acorn's Communicator BASIC for the 65816

The labels roughly match those in 6x09 BASIC, hence some of the rather verbose/odd labels.




# org folder

This folder contains:

 | files        | Descriptions
 |--------------|---------------------------------------------------------------------
 | \*.bin       | the original modules taken from Communicator OS 1.00
 | \*.dis65     | disassembler instruction files for SourceGen
 | \*\_cc65.S   | disassembly produced by SourceGen in ca65 format
 | \*\_cc65.cfg | linker scripts for ld65 to allow rebuild of the disassembled sources


# src folder

This folder contains edited sources to generate new builds of Communicator BASIC for
the following platforms

 * BEEB816 - the BBC Micro 816 board produced by [Revaldinho, BigEd and Hoglet](https://github.com/BigEd/beeb816/wiki)
 * DOSSY - the Dossytronics 816 TUBE for the BBC Micro (Available in B-Em)
 * BLITTER - the 816 CPU in the [Dossytonics Blitter Board](https://github.com/dominicbeesley/blitter-vhdl-6502/wiki)
 * COM* - builds which could be used on a Communicator (untested)

# ssd folder

This folder contains builds for:
 * BLITTER - contains a boot script and small BASIC2/4 program to switch to Native Mode of the 816 and run Communicator BASIC on the Blitter


# build/ARITH100\* and build/COMBAS100\*

These files are a reassembly of the original modules made by reassembling the \*\_cc65.S 
files from the org folder. If all is well with the disassembly files these files should
be identical to the .bin files in the org folder.

# build/arith_new_COMARITH100 build/arith_new_COMBAS100

These files are built from the edited sources in the src folder

# cmp

The files in this folder are comparisons by a da65 disassembly of the files from build/ above that have been diffed with a da65 disassembly of the original binary files - this is a sanity check that the SourceGen and edited sources have not altered the source.



