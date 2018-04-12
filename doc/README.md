# FANTASI documentation

## Architectural overview

Partially done in the paper

## Prerequisites

* FPGA board X
* Quartus v.N
* NIOS devkit
* Pangraph library v.N
* GHC compiler v.N

Including installation notes.

##  Building an accelerator instance

### Generating FPGA bitstream

* Get GraphML file describing the network for analysis.
* Run `fantasi`/script to produce VHDL from GraphML.
* Import VHDL to Quartus, synthesise (expected time: 2 hours)
* Generating NIOS system VHDL (SOPC?)
* Program the FPGA.

### Downloading NIOS Software

* Build NIOS software
* Downloading software to on-board NIOS processor

# Using the accelerator

* Details on using `nios2-terminal`:
  - environments (Eclipse vs. command line)
  - various commands
  - example usage
  - issues?
