#!/bin/sh

V=2.7.12
cd /tmp
curl -LkO https://www.python.org/ftp/python/${V}/Python-${V}.tar.xz
xz -dc Python-${V}.tar.xz  |tar -xf -
cd Python-${V}
./configure --prefix=/local/cse-mlton/python-${V}
make install
cd /tmp
rm -fr Python-${V}

P=/local/cse-mlton/python-${V}/bin/
curl -O https://svn.apache.org/repos/asf/oodt/tools/oodtsite.publisher/trunk/distribute_setup.py
${P}/python2.7 ./distribute_setup.py
${P}/easy_install pip

${P}/pip install -U pip setuptools
${P}/pip install matplotlib
