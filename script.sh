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
        echo "Usage: $0 [graph] [fantasi-tool] [Top-level-entity]"
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

# get top level entity path
top_entity="/`realpath $3`"
if ! [ -e "$top_entity" ]; then
    echo "Top level entity does not exist in this path: $3"
    exit 0
fi

# generating vhdl files from the graph
printf "Executing $fantasi_tool..."
$fantasi_tool $graph_path -n graph.vhdl -s sim-environment.vhdl
printf "done.\n"

# modify top level entity to adapt the infrastructure to the graph
printf "Generic map parameters substitution..."
mv $top_entity $top_entity.bak
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
        print $0; }' $top_entity.bak > $top_entity
printf "done.\n"
