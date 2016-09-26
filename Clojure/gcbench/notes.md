```

#!/usr/bin/python

# ./p.py ... -m 5000

def insert(k, v):
   print k

def r(l, u):
   if l == u: return
   m = int((u-l)/2) + l
   if (u-l < 2):
       insert(l, l)
   else:
       insert(m, m)
       r(m+1, u)
       r(l, m)

m = 10
r(0, m)

```
