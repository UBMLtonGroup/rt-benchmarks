import sys

lines = sys.stdin.readlines()
assert len(lines) % 2 == 0, "input should contain even # of lines"

for l in range(0, len(lines), 2):
    #print lines[l], lines[l+1]
    start = int(lines[l].rstrip().split(':')[4])
    stop  = int(lines[l+1].rstrip().split(':')[4])
    print stop-start
    
