@ECHO OFF
rem id2go annotates proteins with their Gene Ontology terms.
rem An internet connection is required, since GO terms are
rem downloaded from Uniprot

rem USAGE
rem id2go.bat input_file output_file
rem
rem input_file:  a file with protein accession number separted by whitespace
rem              e.g. a list or a network with protein ids.
rem output_file: will be created and contains all protein ids within the
rem              input file annotated with GO terms

rem EXAMPLE
rem id2go.bat data/SC.ppi data/SC.anno

java -Xmx600M -cp "go2ppi.jar"  au.edu.imb.go2ppi.Id2go %1 %2