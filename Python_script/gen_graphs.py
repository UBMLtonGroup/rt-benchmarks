#!/usr/bin/python

import sqlite3

db = sqlite3.connect(":memory:")
cur = db.cursor()
cur.execute("create table stats (type text, action text, tid integer, iter integer, ts real)")

ln = 0

with open("run1.txt") as f:
    for line in f:
        ln += 1
        a = line.rstrip().split(":")

        if len(a) != 5:
            raise Exception("invalid line {}".ln)

        db.execute("insert into stats values ('{}', '{}', '{}', '{}', '{}')".format(a[0],a[1],a[2],a[3],a[4]))
        db.commit()

"""
select s1.type,s1.tid,s1.iter,(s2.ts-s1.ts) from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type where s1.type='compute' and s1.action='start' order by s1.iter;
"""

"""
select s1.type,s1.tid,s1.iter,(s2.ts-s1.ts) from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type where s1.type='gc' and s1.action='start' order by s1.iter;
"""