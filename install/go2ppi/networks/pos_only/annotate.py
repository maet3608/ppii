from glob import glob
from os import system

"""Creates annotation files (.anno) for network files (.ppi) within this
folder by accessing the Uniprot website (Internet connection is required).
Note that the jar file: go2ppi.jar has to be in this folder.
"""

def annotate(filepath):
    args = filepath, filepath.replace(".ppi", ".anno")
    system('java -cp "go2ppi.jar" au.edu.imb.go2ppi.Id2go %s %s'%args)

if __name__ == "__main__":
    for filepath in glob("*.ppi"):
        print filepath
        annotate(filepath)