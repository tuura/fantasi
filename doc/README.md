# FANTASI documentation

This tutorial guides the reader through the usage of the
*FANTASI* (FAst NeTwork Analysis in SIlicon) hardware
accelerator, developed during the EPSRC programme grant
[POETS](https://poets-project.org/) in partnership with [e-Therapeutics
PLC](https://www.etherapeutics.co.uk/). First, we say a few lines about the
[motivation](https://github.com/allegroCoder/fantasi/tree/documentation/doc#motivational-overview)
of the project, then we give an [architectural
overview](https://github.com/allegroCoder/fantasi/tree/documentation/doc#architectural-overview)
of the accelerator, and finally we
show how to build one and use it, see
[prerequisites](https://github.com/allegroCoder/fantasi/tree/documentation/doc#prerequisites)
and [building an accelerator](https://github.com/allegroCoder/fantasi/tree/documentation/doc#building-an-accelerator-instance).
You can jump to the section you're interested in by clicking on the
above links.

## Motivation

This project is motivated by the area of *computational drug discovery*. In
this area, biological systems are modelled by protein-protein interaction (PPI)
networks [[1]](https://github.com/allegroCoder/fantasi/tree/documentation/doc#basic-references) (large-scale graphs), which are analysed for collecting statistics
and information that are used in pharmacological laboratories for a more
effective discovery of drugs. Watch the below video to have a deeper insight
of the drug discovery process, pionereed by *e-Therapeutics PLC*.

<p align="center">
  <a href="http://www.youtube.com/watch?feature=player_embedded&v=wQFpTtuzrgA" target="_blank">
  <img src="http://img.youtube.com/vi/wQFpTtuzrgA/0.jpg" alt="WATCH ME" width="480" height="360" border="10"/></a><br>
  The YouTube video of the drug discovery process. Click the image to play it!
</p>

Conventional computer architectures cannot process such large graphs
efficiently. The latter are stored in wide and slow off-chip memories, which
have to be queried continuously due to irregular memory accesses typical
of graph-related algorithms (e.g. graph traversal). To overcome this issue
imposed by the memory-bandwidth, we designed an hardware accelerator where
a graph is mapped into a digital circuit, where nodes are individual memory
elements (flip-flop) and arcs are combinational paths between these elements,
and is stimulated by an had-hoc infstracture with the purpose of extracting
statistic from the graph such as connectedness, resilience, etc.

<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/speed.png" width="500" border="10"/></a><br>
  Fig. 1. Comparison of network analysis performance.
</p>

Figure 2 shows the scalability of the prototyped FANTASI accelerator
versus a software implementation in C++ for the drug discovery analysis. The
number of active edges (|E|) in a realistic PPI network is varied and
calculated execution time are shown. The plot highlights two aspects:
(a) The accelerator delivers a speed improvement of 2/3 orders of magnitude
depending on |E|. (b) While software execution time scales linearly with |E|,
the accelerator execution time slightly decreases, as the higher connectivity
reduces the network diameter, and also the corresponding time to fully
explore all PPI paths thanks to hardware parallelism.

The FANTASI accelerator is important as it highlights the benefits of
having a network of interconnected processing units, which in this case
are represented by flip-flop registers, that can operate concurrently. It
represents a demonstration, at a much lower scale, of the processing capability
of many-cores architectures, such as the [POETS](https://poets-project.org/).

## Architectural overview

The FANTASI accelerator relies on a mechanical, and
automated, process to map an *application network*
(in this case the Protein to Protein Interaction network
[[1]](https://github.com/allegroCoder/fantasi/tree/documentation/doc#basic-references))
into a digital circuit. Figure 2 shows an example of this process: the
nodes of a network are converted into flip-flop registers, which are
interconnected according to the connections specified in the application
network. The OR gates join multiple incoming connections to a register, and
propagate messages between registers for computing the network resilience,
given by the average shortest path of the network. The latter is important
for the process of drug discovery.

<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/graph.png" width="720" border="10"/></a><br>
  Fig. 2. The process of mapping a graph to a digital circuit.
</p>

The basic idea behind representing graphs using flip-flops and
combinational paths is that we wish to perform graph traversal by
propagating logic high values between flip-flops. The logic state of
each flip-flop therefore indicates whether a given vertex has been
visited (logic high) or not (logic low). To propagate a 'visited' state
between flip-flops, we OR the outputs of all vertex neighbours and use
it as an input to the vertex flip-flop. For further details, refer to
[[2]](https://github.com/allegroCoder/fantasi/tree/documentation/doc#basic-references).

Figure 3, in turn, shows an high-level view of the FANTASI infrastucture. The
digital circuit of the application network is at the core of the
infrastructure, highlighted with a red dashed line. The *hardware graph*
is encapsulated by the control circuitry to enable/disable selected nodes,
coordinate computation, and read shortest-path computation results. An
on-chip software processor (NIOS II) is also included to communicate with
the host computer and provide an Application Programming Interface (API)
for network processing. The basic idea of the FANTASI infrastructure is to allow
the reconfiguration of the network at runtime, in order to run multiple analyisis
on the same network under different configurations, without requiring the network
resynthesis. Each configuration is identified by a set of nodes that are disabled.

<!-- TODO:  write something about the network reconfiguration-->

<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/hw.png" width="840" border="10"/></a><br>
  Fig. 3. The FANTASI hardware accelerator infrastructure.
</p>

For our experiments, we used the *Altera DE4 board* that integrates the
*Stratix IV FPGA*. The host computer connects to the boards via a USB cable
that uses the JTAG UART interface. This is used for downloading the C code to
the NIOS II soft-processor, which provides the API for graph processing. The
host computer, instead, interfaces with the user of the FANTASI accelerator.

## Prerequisites

Here is what you need to build and use the `fantasi` tool of this repository:

* [**GHC compiler 8.2.2 or above**](https://www.haskell.org/ghc/) - the
Glasgow Haskell compiler is needed to compile and build the `fantasi` tool,
which generates the hardware graph in the form of VHDL.
* [**Pangraph library v0.1.1.5**](https://hackage.haskell.org/package/pangraph) -
The pangraph library, available on hackage on the link provided, is needed
to the `fantasi` tool for parsing an application graph (in GraphML format).
* [**Stack**](https://docs.haskellstack.org/en/stable/README/) - the simplest
way to install the `fantasi` tool is by using Stack. It can install GHC, and
download all the packages needed to building the `fantasi` tool automatically.


For installing the `fantasi` tool globally, you need to run the following command.
```
stack install
fantasi --help
```

For local install:
```
stack build
stack exec fantasi -- --help
```

The second command `fantasi --help` displays the help of the tool, which is shown below:
```
Usage: fantasi [graphml file]
  -n FILEPATH  --graph-name=FILEPATH  VHDL graph output file-path
  -s FILEPATH  --sim-name=FILEPATH    VHDL simulation environment output file-path
  -h           --help                 Show this help message
  -v           --version              Show version of Fantasi
```

To use the FANTASI hardware accelerator, on the other hand, you will need the following:

* **An Altera FPGA board** - in our experiments, we used the Altera DE4
board that embeds the Stratix IV FPGA. However, any FPGA by Altera can be
used.
* [**Quartus Prime
Software**](https://www.altera.com/downloads/download-center.html) - the
Altera design environment for the usage of any Altera FPGA board. We
suggest to download the latest version of the software currently available. The
lite edition version enables the compilation and synthesis of hardware into some
of the Altera FPGA boards available. However, a license is required for the usage
of the latest and bigger FPGA, such as the one that we used.
* [**NIOS II
devkit**](https://www.altera.com/products/processors/overview.html) -
the NIOS II is a software processor that can be synthesised in any Altera
FPGA. The development kit available with Quartus can be used to customise
and synthesise the processor into the FPGA.

For more information about how to install the above tool, refer to the official
documentation.

## Building an accelerator instance

In the previous section of the tutorial, we described the materials that are
needed to start using the FANTASI accelerator. In this section, we describe
a step-by-step guide that show how it can be used.


### Generating FPGA bitstream

* **1.** Get a [GraphML](http://graphml.graphdrawing.org/) file describing the network for analysis. As an example, the application network in Figure 2 is described as below:
```
<?xml version="1.0" encoding="UTF-8"?>
<graphml>
  <graph id="G" edgedefault="undirected">
    <node id="A"/>
    <node id="B"/>
    <node id="C"/>
    <node id="D"/>
    <edge source="A" target="C" directed="false"/>
    <edge source="C" target="B" directed="false"/>
    <edge source="B" target="D" directed="false"/>
    <edge source="A" target="D" directed="false"/>
  </graph>
</graphml>
```

* **2.** The NIOS development kit can be used from within Quartus to generate
a VHDL entity of the NIOS II software processor. For simplicity, we provide
all the VHDL dependencies needed to generate the FPGA bitstream of the FANTASI
accelerator, including the top level entity (`TOP.vhdl`) that includes the
NIOS II processor and the instance of the FANTASI infrastructure. This file
needs to be customised according to the network that one wants to analyse
(see next iteration).

* **3.** Run the Bash script `fantasi-script.sh` to generate the hardware
network (Figure 2), the FANTASI infrastructure for the drug discovery analysis
(Figure 3), and for modifying the Top level entity that we are going to
synthesise into the FPGA.

```
./fantasi-script.sh [graphml file] [fantasi tool] [top level VHDL entity]
```
The above command will produce the `graph.vhdl` file, which contains the
hardware graph in VHDL, and the `sim-environment.vhdl` file, that contains the
FANTASI infrastructure. In addition, it will modify the VHDL file specified
as top level entity (in this case `vhdl/TOP.vhd`), according to the network
characteristics.

* **4.** Create a new project in Quartus and select the FPGA that
you want to use. And then import the generated files (`graph.vhdl` and
`sim-environment.vhdl`), and the content of the `vhdl/` folder into the Quartus
project. Finally, set the file `TOP.vhd` as top level entity of the project.

* **5.** Run the compilation process in Quartus, and then program the
generated bistream file into the FPGA.

As a result, the FANTASI accelerator and the Nios II software processor
should be mapped onto the FPGA that you are using.

### Downloading NIOS Software

If you followed the previous steps, now you should have the FANTASI
accelerator and the NIOS II processor mapped inside the FPGA of the board
that you are using. In this step, we will download the C-based API into the
NIOS II processor that enables the usage of the FANTASI accelerator from an
external host computer. To do this, we will use the *Eclipse* development
environment plugin embedded within Quartus. In the version of Quartus that
we have used (16.1), `Eclipse` can be found under the tab `Tools -> Nios II
Software Build Tools for Eclipse`.

* **1.** In the repository, you can find the C-based API ready to compile,
build and download into the Nios II processor, see file `fantasi-API.c`. To
use this API, first you will need to create a project inside the `Nios
II Software Build Tools for Eclipse` plugin. The project that is created
is divided in 2 sub-projects: (1) the first one contains tha application
that you want to run in the Nios II processor, (2) the second one has
as suffix '_BSP', which stands for Board Support Package, and contains
the low level drivers that are needed to interface with the board that
one is using. To create this two projects, click on the tab `File ->
Nios II Application and BSP from Template` from within `Eclipse` design
environment. A window (see Fig. 4) will be opening, where you need to
select the SOPC file (select the file `fantasi-niosII.sopcinfo` of the repo,
see note below), write a name for the project (let's call it 'FANTASI'),
and choose a project template (select the 'Blank Project'). The SOPC file
is created by the Nios II Development kit (see Point 2 of [Generating FPGA
bitstream](https://github.com/allegroCoder/fantasi/tree/documentation/doc#generating-fpga-bitstream)).

<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/nios-project.png" width="450" border="10"/></a><br>
  Fig. 4. Nios II Application and BSP from Template.
</p>

*Note:* the file 'fantasi-niosII.sopcinfo' provided in this repository works
specifically with the FPGA (Stratix IV EP4SGX230KF40C2). If you want to
use a different board, you will have to generate the Nios II processor by
following this tutorial: [Generate your own Nios II processor](https://github.com/allegroCoder/fantasi/tree/documentation/doc#generate-your-own-nios-ii-processor).

* **2.** Now that you created the projects, you have to insert the file
`fantasi-API.c`, provided inside the repository, inside the 'FANTASI'
(non 'FANTASI_BSP') project. This file contains the API for accessing the
HW accelerator, and it is ready to be compiled, built and downloaded into
the Nios II processor for its execution.

* **3.** The next step is to create the 'Run configuration' properties
within `Eclipse` for building the project and download it into the Nios
II processor. To do this, click on `Run -> Run Configurations...` (see
Fig. 5). Next, right-click on the tab group `Nios II Hardware -> New`. Under
the tab 'Project', select the 'FANTASI' project in `Project name`, and
select the executable file with the extension '.elf', which is present
within the 'FANTASI' project in Eclipse upon compilation. Under the tab
'Target Connection', enable the following checkboxes: `Ignore mismatched
system ID` and `Ignore mismatched system timestamp`. You should see the
board within the `Connections` section under this tab, if not, try to
`Refresh connections`. Finally, click `Apply` and `Run`.

<p align="center">
  <img src="https://github.com/allegroCoder/fantasi/blob/documentation/doc/fig/nios-configurations.png" width="750" border="10"/></a><br>
  Fig. 5. Eclipse window 'Run Configurations'.
</p>

As a result, the API interface should be now up and running. The Eclipse
plugin GUI will display the console of the Nios II processor in the `Console`
section. Read through the next section for some instructions on how to use
the accelerator.

## Using the accelerator

**How to run commands on the FANTASI API?** At this point, the Nios II processor is ready to accept commands for running drug discovery analysis over the synthesised network. The simplest way to use the accelerator is via the `Console` within Eclipse environment. Otherwise, one could use the tool `nios2-terminal` (provided inside the Quartus installation folder `~/intelFPGA/[Quartus-version]/quartus/[OS-used]/`) to connect the terminal to the FPGA board, and to run the FANTASI API provided. If you want to use the terminal, you need to run the `nios2-terminal` in order to be prompted to use the terminal as FANTASI command line.

**Which commands are available?** Find below the help of the FANTASI API.
```
Commands:
  start                          Run a simulation with all the nodes enabled
  test-drug 'n' -- [off nodes]   Run a simulation with the following 'n' nodes disabled in the form of '5 100 1 305'
                                   Example: test-drug 3 -- 5 10 946
  random 'n' 'm'                 Run 'n' simulations, removing 'm' random nodes at every run
  random-set 'from' 'to' 'n'     Remove 'n' independently chosen sets of size k, for k = 'from' .. 'to'
  growing-set 'to' 'n'           Remove 'n' growing sets of size k, for k = 1 .. 'to', starting
                                   with fixed random permutations as seeds for the growing sets
  force 'to'                     Remove sets of size k, for k = 1 .. 'to', where each node added into the
                                   set maximises the impact of the network
  result                         Display the result
  reset                          Reset the network, the register that contain the result, and the shift register
  stat                           Print number of nodes of the network
  help                           Print help of the tool
```

**Example of usage:**
For example, by running `start`, you will compute the average shortest path
of the networks with all the nodes enabled. With the command `test-drug 3 --
10 60 99`, you will compute the average shortest path (and the impact on the
resilience of the network) when the nodes with indices {10, 60, 99} are off.

## Generate your own Nios II processor

<!-- TODO: guide on how to generate the Nios II processor from within Quartus -->

## Basic references

[1] M. P. Young et al. *Chapter 3. Drug Molecules and Biology: Network and Systems Aspects*, in RSC Drug Discovery, pp. 32-49, 2012.

[2] A. Mokhov et al. *Language and Hardware Acceleration Backend for Graph Processing*, Forum on specification & Design Languages, 2017.
