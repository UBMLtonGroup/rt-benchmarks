
# coding: utf-8

# In[1]:

import sqlite3
import numpy as np
import matplotlib as mp
import matplotlib.pyplot as plt
import matplotlib.patches as patches

#get_ipython().magic(u'matplotlib inline')


# In[2]:

def select_by(db, name, type):

    S = """select s1.ts,s1.type,s1.tid,s1.iter,(s2.ts-s1.ts),s1.heap,s2.heap,s1.details
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='{type}' and s1.action='start' and s1.name = '{name}' and s2.name = '{name}'
 order by s1.ts asc;"""
    cur = db.cursor()
    r = []
    #print S.format(type=type, name=name)
    for row in cur.execute(S.format(type=type, name=name)):
        r.append(row)
    return r

def csv(a):
    for i in a:
        print ",".join(str(x) for x in i)


# In[3]:

db = sqlite3.connect("stats.sql")
DS = ''
a = select_by(db, '20170430-hask-gcb', 'compute')
b = select_by(db, '20170430-hask-gcb', 'gc')
c = a + b
c.sort(key=lambda a: a[0])
#csv(c)
db.close()


# In[4]:

for i in range(10):
    print a[i]


# In[10]:

# haskell time is in us .. div by 1000 for ms
xs = [(x[0]-a[0][0])/(10^3) for x in a]
ys = [x[4]/(10^3) for x in a]
mean0, mean1 = np.mean(ys[:30]), np.mean(ys[31:100])
std0, std1 = np.std(ys[:30]), np.std(ys[31:100])
#print mean0, std0
#print mean1, std1
#print xs[:10]
plt.yscale('log')
L0 = "$\mu$={0:.2f},\n$\sigma={1:.2f}$".format(mean0, std0)
L1 = "$\mu$={0:.2f},\n$\sigma={1:.2f}$".format(mean1, std1)
#print L
plt.xlabel(r"Delta Start Time ($ms$)")
plt.ylabel("Run Time ($ms$)")
plt.plot(xs[:100], ys[:100], 'bs')
plt.text(0.0, mean0*2, L0)
plt.text(xs[50], mean1, L1)
plt.title(a[0][7])
sub = plt.gca()
sub.add_patch(
    patches.Rectangle(
        (0.0, 200),   # (x,y)
        10**6,          # width
        6000,          # height
    )
)
plt.show()





