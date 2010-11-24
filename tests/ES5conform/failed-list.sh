#!/bin/bash
outfile="failed-listing"

cat failed |
while read line
do
echo "$line"
fname=`find TestCases -type f -name "$line"`
contents=`cat $fname`
echo "===$line===" >> $outfile
echo "$contents" >> $outfile
echo "\n\n" >> $outfile
done