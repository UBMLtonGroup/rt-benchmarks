#!/bin/sh 

echo "Installing erlang.."
cd /tmp
curl -O http://erlang.org/download/otp_src_19.2.tar.gz
tar -zxf otp_src_19.2.tar.gz
cd otp_src_19.2
./configure --prefix=/local/cse-mlton
make
make install
cd /tmp
rm -fr otp_src_19.2



