#!/bin/sh 

echo "Installing git.."

cd /tmp
URL=https://github.com/git/git/archive/v2.11.0.tar.gz
B=`basename $URL`
curl -LkO  $URL
tar -zxf $B
cd git-2.11.0
make prefix=/local/cse-mlton all 
make prefix=/local/cse-mlton install 
cd /tmp
rm -fr git-2.11.0



