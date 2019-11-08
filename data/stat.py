"""Calculates number of proteins and interactions for PPI datasets"""

from glob import glob

def stats(filename):
    def partners(line): return [e.strip() for e in line.split()[:2]]
    with open(filename) as f:
        interactions = [partners(line) for line in f]
    proteins = set()
    for p1,p2 in interactions:
        proteins.add(p1); proteins.add(p2)
    return len(proteins), len(interactions)

def main():
    filenames = glob("??_*.txt")
    for filename in filenames:
        np,ni = stats(filename)
        print "%20s %4d %5d"%(filename, np, ni)
        

if __name__ == "__main__":
    main()