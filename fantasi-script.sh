#!/bin/bash

# log2 function
function log2 {
    local x=0
    for (( y=$1-1 ; $y > 0; y >>= 1 )) ; do
        let x=$x+1
    done
    echo $x
}

# help of the tool
for var in "$@"
do
    if [ $var = "-h" -o $var = "-help" -o $var = "--help" -o $var = "-H" ]; then
        echo "This is a script for parsing a graph (XML format), converting it into"
        echo "hardware, and synthesing it into an Altera FPGA board."
        echo "Usage: $0 [graph] [pangraph-tool] [Quartus-project-dir]"
        exit 0
    fi
done

# get graph path
graph_path="/`realpath $1`"
if ! [ -e "$graph_path" ]; then
    echo "Graph does not exist in this path: $1"
    exit 0
fi

# get fantasi tool path
fantasi_tool="/`realpath $2`"
if ! [ -e "$fantasi_tool" ] || ! [ -x "$fantasi_tool" ]; then
    echo "Fantasi tool does not exist in this path: $2"
    exit 0
fi

# get Quartus project folder
quartus_project_folder="/`realpath $3`"
if ! [ -d "$quartus_project_folder" ]; then
    echo "$3 is not a directory"
    exit 0
fi

if ! [ -e "$quartus_project_folder/TOP.vhd" ]; then
    echo "Top level entity not found in $3. It should be named TOP.vhd"
    exit 0
fi

# generating vhdl files from the graph
printf "Executing $fantasi_tool..."
$fantasi_tool $graph_path -n $quartus_project_folder/graph.vhdl \
-s $quartus_project_folder/sim-environment.vhdl
printf "done.\n"

# modify top level entity to adapt the infrastructure to the graph
printf "Generic map parameters substitution..."
mv $quartus_project_folder/TOP.vhd $quartus_project_folder/TOP.vhd.bak
NODES=`cat $graph_path | grep "<node" | wc -l`
ADDR_BITS=$(log2 $NODES)
RESULT_BITS=$((($ADDR_BITS * 2) + 1))

awk -v nodes=$NODES -v result_bits=$RESULT_BITS -v addr_bits=$ADDR_BITS \
    '{if ( $1 == "NODES" && $4 == ":=" )
        printf("\t\tNODES : integer := %d;\n", nodes);
    else if ( $1 == "RESULT_WIDTH" && $4 == ":=" )
        printf("\t\tRESULT_WIDTH : integer := %d;\n", result_bits);
    else if ( $1 == "ADDR_SHIFT" && $4 == ":=" )
        printf("\t\tADDR_SHIFT : integer := %d);\n", addr_bits);
    else
        print $0; }' $quartus_project_folder/TOP.vhd.bak > $quartus_project_folder/TOP.vhd
printf "done.\n"
