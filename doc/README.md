# FANTASI documentation

This tutorial guides the reader through the usage of the
*FANTASI* (FAst NeTwork Analysis in SIlicon) hardware
accelerator, developed during the EPSRC programme grant
[POETS](https://poets-project.org/) in partnership with [e-Therapeutics
PLC](https://www.etherapeutics.co.uk/). First, we provide a [motivational
overview](https://github.com/allegroCoder/fantasi/tree/documentation/doc#motivational-overview)
of the project, then we give an [architectural
overview](https://github.com/allegroCoder/fantasi/tree/documentation/doc#architectural-overview)
of the accelerator, and finally we
show how to build one and use it, see [Building an
accelerator](https://github.com/allegroCoder/fantasi/tree/documentation/doc#building-an-accelerator-instance).
Feel free to jump to the section you're interested in by clicking on the
above links.


## Motivational overview

This project is motivated by the domain of *computational drug discovery*. In
this area, biological systems are modelled by protein-protein interaction
networks [1] (large-scale graphs), which are analysed for collecting statistics
and information that are used in pharmacological laboratories for a more
effective discovery of drugs. Watch the below video to have a deeper insight
of the drug discovery process, pionereed by *e-Therapeutics PLC*.


<p align="center">
  <a href="http://www.youtube.com/watch?feature=player_embedded&v=wQFpTtuzrgA" target="_blank">
 <img src="http://img.youtube.com/vi/wQFpTtuzrgA/0.jpg" alt="WATCH ME" width="480" height="360" border="10"/></a><br>
  The YouTube video of the drug discovery process.
</p>

Conventional computer architectures cannot process such large graphs
efficiently. The latter are stored in wide and slow off-chip memories, which
have to be queried continuously due to irregular memory accesses typical
of graph-related algorithms (e.g. graph traversal). To overcome this issue
imposed by the memory-bandwidth, we designed an hardware accelerator where
a graph, composed by flip-flop registers (nodes) interconnected by wires
(arcs), is mapped into silicon and is stimulated by an had-hoc infstracture
with the purpose of extracting statistic from the graph such as connectedness,
resilience, etc.


<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/speed.png" width="520" height="400" border="10"/></a><br>
  Comparison of network analysis performance.
</p>

The above figure shows the scalability of the prototyped accelerator
versus a software implementation in C++. The number of active edges (|E|)
in a realistic PPI network is varied and calculated execution time are
shown. The plot highlights two aspects: (a) The accelerator delivers a speed
improvement of 2/3 orders of magnitude depending on |E|. (b) While software
execution time scales linearly with |E|, the accelerator execution time
slightly decreases, as the higher connectivity reduces the network diameter,
and also the corresponding time to fully explore all PPI paths thanks to
hardware parallelism.

The FANTASI accelerator is also important as it highlights the benefits of
having a network of interconnected processing core units, which in this case
are represented by flip-flop registers, that can operate concurrently. It
represents a demonstration, at a much lower scale, of the processing capability
of many-cores architectures, such as the [POETS](https://poets-project.org/).

## Architectural overview

Partially done in the paper

## Prerequisites

* FPGA board X
* Quartus v.N
* NIOS devkit
* Pangraph library v.N
* GHC compiler v.N

Including installation notes.

## Building an accelerator instance

### Generating FPGA bitstream

* Get GraphML file describing the network for analysis.
* Run `fantasi`/script to produce VHDL from GraphML.
* Import VHDL to Quartus, synthesise (expected time: 2 hours)
* Generating NIOS system VHDL (SOPC?)
* Program the FPGA.

### Downloading NIOS Software

* Build NIOS software
* Downloading software to on-board NIOS processor

## Using the accelerator

* Details on using `nios2-terminal`:
  - environments (Eclipse vs. command line)
  - various commands
  - example usage
  - issues?

## Basic references

[1] M. P. Young et al. *Chapter 3. Drug Molecules and Biology: Network and Systems Aspects*, in RSC Drug Discovery, pp. 32-49, 2012.
