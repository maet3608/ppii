@ECHO OFF
rem gen_filters creates filter files from annotation file.
rem 
rem The prediction phase of go2ppi can take a very long time.
rem To shorten the overall computation time the prediction phase
rem can be distributed on multiple nodes/computers by providing
rem a filter file for each node. The filter file contains a list
rem of protein pairs to compute predictions for. gen_filters 
rem generates filter files by dividing the list of all possible 
rem protein pairings into the given number of filter files.
rem The individual filter files (and go2ppi) are distributed on
rem different nodes/computers and go2ppi creates independent 
rem predictions based on the filter file.

rem USAGE
rem gen_filters annotation_file nr_filter_files output_path
rem
rem annotation_file:  a file with protein accession number followed (tab-separated)
rem                    by their GO annotations (comma-separated)
rem nr_filter_files:  number of filter files to create.
rem output_path    :  path where filter files will be written to, e.g. ./filters


rem EXAMPLE
rem gen_filters.bat test/test.anno 5 ./test/filters

java -Xmx600M -cp "go2ppi.jar"  au.edu.imb.go2ppi.GenerateFilters %1 %2 %3