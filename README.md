[![Build Status](https://travis-ci.org/tuura/fantasi.svg?branch=CI-travis)](https://travis-ci.org/tuura/fantasi)

See [Documentation](https://github.com/tuura/fantasi/tree/master/doc).

# fantasi
`Fantasi` translates graphs from the GraphML format to VHDL, and also creates an infrastructure for their analysis. It relies on `pangraph` (https://github.com/tuura/pangraph.git) for the parsing of the GraphML files.

## Install and usage with stack

For a global install
```
stack install
fantasi n1.graphml
```
Or locally
```
stack build
stack exec fantasi n1.graphml
```

## Usage
```
Usage: Fantasi [graphml file]
  -n FILEPATH  --graph-name=FILEPATH  VHDL graph output file-path
  -s FILEPATH  --sim-name=FILEPATH    VHDL simulation environment output file-path
  -h           --help                 Show this help message
  -v           --version              Show version of Fantasi
```
