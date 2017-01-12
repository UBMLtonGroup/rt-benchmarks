#!/bin/sh

D=/local/cse-mlton
echo "Making $D"
[ -d $D ] && exit 0
mkdir $D
chgrp cse-mlton $D
chmod g+sw $D
mkdir -p $D/bin $D/lib $D/man $D/include $D/share

