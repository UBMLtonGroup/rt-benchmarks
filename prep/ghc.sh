#!/bin/sh 

# curl -O https://downloads.haskell.org/~ghc/7.8.4/ghc-7.8.4-src.tar.xz
# curl -O https://downloads.haskell.org/~ghc/8.0.2/ghc-8.0.2-src.tar.xz

# rebuilt for RH5.8 and /local/cse-mlton

echo "Installing GHC..."

URL=https://buffalo.box.com/shared/static/uon1u0lezt8ty5x96p4zpisxjdukw2sk.bz2
curl -Lko /tmp/ghc.tar.bz2 $URL
tar -jxC /local/cse-mlton/ -vf /tmp/ghc.tar.bz2 
rm -f /tmp/ghc.tar.bz2
cd /local/cse-mlton/bin
for i in ../ghc-8.0.1/bin/* ; do ln -s $i ; done

cd /tmp
curl -LkO https://www.haskell.org/cabal/release/cabal-1.24.2.0/Cabal-1.24.2.0.tar.gz
tar -zxf Cabal-1.24.2.0.tar.gz
cd Cabal-1.24.2.0

ghc -threaded --make Setup
./Setup configure --user --prefix=/local/cse-mlton
./Setup build
./Setup install

cd /tmp
git clone https://github.com/haskell/cabal.git
cd cabal/cabal-install
PREFIX="/local/cse-mlton" ./bootstrap.sh

cabal --config-file=/local/cse-mlton/.cabal2/config update
cabal --config-file=/local/cse-mlton/.cabal2/config install optparse-applicative

[ ! -x ~/.cabal ] && ln -s /local/cse-mlton/.cabal ~/.cabal/
