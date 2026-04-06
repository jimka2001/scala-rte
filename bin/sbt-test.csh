#!/bin/csh -f

alias STDERR 'bash -c "cat - 1>&2"'

setenv LANG en_US.UTF-8
setenv LC_CTYPE en_US.UTF-8
setenv LC_ALL en_US.UTF-8
setenv LANGUAGE en_US.UTF-8

set tmp=/tmp/$USER/$0/$$
echo tmp = $tmp
mkdir -p $tmp
set project=scala-rte
set repo=git@gitlab.lre.epita.fr:jnewton/${project}.git
cd $tmp
git clone $repo
cd $tmp/$project
git checkout lre-master
sbt compile
sbt test



