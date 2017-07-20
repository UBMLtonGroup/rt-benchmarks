import sys
m = 0
for l in sys.stdin:
    try:
        h = (l.rstrip().split(':'))[5]
        if h > m: m = h
    except:
        pass

print m

