#!/bin/csh -f
alias STDERR 'bash -c "cat - 1>&2"'

echo ============== running  scala tests ==============

echo pwd= `pwd`
set tmpfile = sbt.$$.out
sbt -Dsbt.log.noformat=true "set Test / parallelExecution := false" test |& tee $tmpfile
grep -e '^[[]error[]]' $tmpfile
if ($status == 0) then
  echo error running tests  | STDERR
  exit 1
endif
