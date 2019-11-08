Version 1.0
Date: 29.08.2011

All files with the extension .ppi are protein protein interaction (PPI) networks as edge lists.
Protein identifiers are Unitprot accession numbers.
All files with the extension .anno are annotation files that contain the GO annotations for the
proteins within the network. 

The differences to the PPI networks in the pos_neg folder are that the
networks in this folder are
1) contain positive interactions only
2) extracted from the experimental AND the database channel
3) GO annotation is provided
=> The networks here are larger and in a format suited for go2ppi.

Legend
  SC: Saccharomyces cerevisiae 
  HS: Homo sapiens
  MM: Mus musculus
  AT: Arabidopsis thaliana
  DM: Drosophila melanogaster
  SP: Schizosaccharomyces pombe
  EC: Escherichia coli
  CT: Chlamydia trachomatis

Data sets were extracted from the STRING database Version 9 
and filtered for interactions from the experimental and database channels only 
with a confidence score >= 0.9. Also, redundant interactions, self-interactions 
and interaction without a "binding action" tag were removed.
