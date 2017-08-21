#!/bin/sh

yum groupinstall 'Development Tools'

yum install openssl-devel curl-devel \
expat-devel perl-ExtUtils-MakeMaker.noarch \
git-all java-1.8.0-openjdk-devel  java-1.8.0-openjdk-javadoc 

