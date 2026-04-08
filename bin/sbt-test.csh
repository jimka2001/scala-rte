#!/bin/csh -f

alias STDERR 'bash -c "cat - 1>&2"'

setenv LANG en_US.UTF-8
setenv LC_CTYPE en_US.UTF-8
setenv LC_ALL en_US.UTF-8
setenv LANGUAGE en_US.UTF-8

if ($#argv) == 1 then
  set branch=$argv[1]
else
  set branch=lre-master
endif

set tmp=/tmp/$USER/$0/$$
echo tmp = $tmp
mkdir -p $tmp
set project=scala-rte
set repo=git@gitlab.lre.epita.fr:jnewton/${project}.git
cd $tmp
git clone $repo
cd $tmp/$project
git checkout $branch
if ($status != 0) then
  echo cannot checkout branch $branch | STDERR
  exit 1
endif
sbt compile
if ($status != 0) then
  echo failed to compile | STDERR
  exit 2
endif
sbt test



