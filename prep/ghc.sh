#!/bin/sh 

# rebuilt for RH5.8 and /local/cse-mlton

echo "Installing GHC..."

URL=https://buffalo.box.com/shared/static/uon1u0lezt8ty5x96p4zpisxjdukw2sk.bz2
curl -Lko /tmp/ghc.tar.bz2 $URL
tar -jxC /local/cse-mlton/ -vf /tmp/ghc.tar.bz2 
rm -f /tmp/ghc.tar.bz2
cd /local/cse-mlton/bin
for i in ../ghc-8.0.1/bin/* ; do ln -s $i ; done

