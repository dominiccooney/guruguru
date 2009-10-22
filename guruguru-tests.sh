#!/bin/bash
EMACS=emacs
OPTIONS="-L . -L third_party"
OUTPUT=/tmp/.el-expectations
$EMACS -q --no-site-file --batch $OPTIONS -l el-expectations -f batch-expectations $OUTPUT guruguru-tests.el
ret=$?
cat $OUTPUT
rm $OUTPUT
exit $ret
