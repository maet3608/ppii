@ECHO OFF
rem go2ppi predicts protein-protein interaction networks from
rem proteins annotated by their Gene Ontology terms.

rem USAGE
rem go2ppi.bat config_file

rem EXAMPLE
rem go2ppi.bat go2ppi.cfg

java -Xmx600M -Xss100M -cp "go2ppi.jar"  au.edu.imb.go2ppi.Go2ppi %*