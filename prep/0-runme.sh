#!/bin/sh -x

echo "Setting us up the locals..."

for i in mkdir git erlang ghc scala ; do 
   echo $i
   sh $i
done

chmod -R g+w /local/cse-mlton

