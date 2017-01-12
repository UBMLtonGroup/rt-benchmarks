#!/bin/sh 

X=`java -version 2>&1| grep version | cut -d \  -f 3 | cut -c 2-4`
echo "Found java $X"
if [ "$X " != "1.8 " ] ; then
    echo <<EOF
Goto http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html

install JDK8, make sure 'java -version' says 8, then re-run
this script
EOF
    exit 255
fi



echo "Downloading Scala..."
VER=2.12.1

curl -Lko /tmp/scala.tar.gz https://github.com/scala/scala/archive/v${VER}.tar.gz
cd /local/cse-mlton
tar -zxf /tmp/scala.tar.gz
git clone https://github.com/paulp/sbt-extras
export PATH=/local/cse-mlton/sbt-extras/bin:$PATH

echo "Building Scala..."
cd /local/cse-mlton/scala-${VER}
sbt compile
sbt dist/mkPack
sbt dist/mkBin

echo "Linking Scala..."
cd /local/cse-mlton/bin
ln -s ../scala-${VER}/build/pack/bin/scalap
ln -s ../scala-${VER}/build/pack/bin/scalac
ln -s ../scala-${VER}/build/pack/bin/scala
 
echo "Testing Scala..."
scala -version

rm /tmp/scala.tar.gz
