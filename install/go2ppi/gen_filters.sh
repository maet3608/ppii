#!/bin/bash
# gen_filters creates filter files from annotation file.
# 
# The prediction phase of go2ppi can take a very long time.
# To shorten the overall computation time the prediction phase
# can be distributed on multiple nodes/computers by providing
# a filter file for each node. The filter file contains a list
# of protein pairs to compute predictions for. gen_filters 
# generates filter files by dividing the list of all possible 
# protein pairings into the given number of filter files.
# The individual filter files (and go2ppi) are distributed on
# different nodes/computers and go2ppi creates independent 
# predictions based on the filter file.

# USAGE
# gen_filters annotation_file nr_filter_files output_path

# annotation_file:  a file with protein accession number followed (tab-separated)
#                    by their GO annotations (comma-separated)
# nr_filter_files:  number of filter files to create.
# output_path    :  path where filter files will be written to, e.g. ./filters

# EXAMPLE
# gen_filters.bat test/test.anno 5 ./test/filters

export GO2PPI=`dirname $0`
java -Xmx600M -cp "$GO2PPI/go2ppi.jar"  au.edu.imb.go2ppi.GenerateFilters $1 $2 $3
