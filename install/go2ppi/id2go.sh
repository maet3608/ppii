#!/bin/bash
# id2go annotates proteins with their Gene Ontology terms.
# An internet connection is required, since GO terms are
# downloaded from Uniprot

# USAGE
# id2go.bat input_file output_file
#
# input_file:  a file with protein accession number separted by whitespace
#              e.g. a list or a network with protein ids.
# output_file: will be created and contains all protein ids within the
#              input file annotated with GO terms

# EXAMPLE
# id2go.bat data/SC.ppi data/SC.anno

export GO2PPI=`dirname $0`
java -Xmx600M -cp "$GO2PPI/go2ppi.jar" au.edu.imb.go2ppi.Id2go $1 $2
