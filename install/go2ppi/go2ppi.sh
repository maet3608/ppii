#!/bin/bash
# go2ppi predicts protein-protein interaction networks from
# proteins annotated by their Gene Ontology terms.

# USAGE
# go2ppi.bat config_file

# EXAMPLE
# go2ppi.bat go2pii.cfg

export GO2PPI=`dirname $0`
java -Xmx600M -Xss100M -cp "$GO2PPI/go2ppi.jar"  au.edu.imb.go2ppi.Go2ppi $@
