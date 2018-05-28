import sqlite3
import argparse


def main(args):
    db = sqlite3.connect(args.db)

    cur = db.cursor()
    cur.execute("create table IF NOT EXISTS stats (name text, details text, cmd text, type text, action text, tid integer, iter integer, ts real, heap integer)")
    cur.execute("delete from stats where name = '{}'".format(args.desc));

    ln = 0
    cmd = ""
    title = ""

    with open(args.file) as f:
        for line in f:
            if line.startswith('#C'):
                cmd = line[3:].rstrip()
                continue
            if line.startswith('#T'):
                title = line[3:].rstrip()
                continue

            ln += 1
            a = line.rstrip().split(":")
            a = map(lambda x: x.strip(), a)

            if 6 >= len(a) >= 5:

                if args.debug is True:
                    print "{} parts in line {}".format(len(a), line)

                if a[4].endswith('s'): # Haskell format is ####.###s
                    a[4] = a[4][:-1]
                db.execute("insert into stats values ('{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}', '{}')".format(args.desc, title, cmd, a[0],a[1],a[2],a[3],a[4],a[5]))
            else:
                pass #print "skip"
        db.commit()

    db.close()

def cla():
    parser = argparse.ArgumentParser()
    parser.add_argument('-d','--db', help='database file (default stats.db)', default = 'stats.db')
    parser.add_argument('-f','--file', help='input file')
    parser.add_argument('-t','--desc', help='data file description')

    parser.add_argument('-D','--debug', action = 'store_true', help='Enable debugging output' , default = False)
    return parser.parse_args()

if __name__ == "__main__":
    main(cla())
