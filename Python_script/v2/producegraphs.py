import sqlite3
import argparse
import matplotlib as mp



S = """select s1.ts,s1.type,s1.tid,s1.iter,(s2.ts-s1.ts),s1.heap,s2.heap
  from stats s1 inner join stats s2 on s1.tid=s2.tid and s1.iter=s2.iter and s1.action!=s2.action and s1.type=s2.type
 where s1.type='{type}' and s1.action='start' and s1.name = '{name}' and s2.name = '{name}'
 order by s1.iter desc;"""


def main(args):
    db = sqlite3.connect(args.db)
    a = select_by(db, '20170321-gcbench-run6', 'compute')
    b = select_by(db, '20170321-gcbench-run6', 'gc')
    c = a + b
    c.sort(key=lambda a: a[0])
    csv(c)
    db.close()

def csv(a):
    for i in a:
        print ",".join(str(x) for x in i)

def select_by(db, name, type):
    cur = db.cursor()
    r = []
    for row in cur.execute(S.format(type=type, name=name)):
        r.append(row)
    return r

def cla():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d','--db', help='database file (default stats.db)', default = 'stats.db')

    parser.add_argument('-D','--debug', action = 'store_true', help='Enable debugging output' , default = False)
    return parser.parse_args()

if __name__ == "__main__":
    main(cla())
