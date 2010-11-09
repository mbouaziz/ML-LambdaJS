#!/bin/bash

P=`dirname $0`

for FILE in $P/*.js
do
  $P/../../jsc.native $FILE
done
