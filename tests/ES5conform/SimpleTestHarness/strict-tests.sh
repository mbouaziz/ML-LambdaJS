#!/bin/bash

P=`dirname $0`
failures=$((0))
numTests=$((0))
newFailed=$((0))
newPassed=$((0))
failedFile="../failed"
passedFile="../passed"
tempFailedFile=`mktemp -t ljses5.XXXXX`
tempPassedFile=`mktemp -t ljses5.XXXXX`
failedString="\e[0;31mFAILED\e[0m"
passedString="\e[0;32mPASSED\e[0m"
unknownString="\e[0mUNKNOWN\e[0m"
wasBefore=""

function was {
    F=$1
    w=0 #failed=0, unknown=1, passed=2
    failedBefore=`cat $failedFile | grep $F`
    passedBefore=`cat $passedFile | grep $F`
    if [[ $failedBefore != "" ]];
    then
	w=0
	wasBefore=$failedString
    else
	if [[ $passedBefore != "" ]];
	then
	    w=2
	    wasBefore=$passedString
	else
	    w=1
	    wasBefore=$unknownString
	fi
    fi
    return $w
}

function testResult {
    retcode=$2
    file=`basename $1`
    was $file
    w=$?
    if [[ $retcode == 1 ]] 
    then
	#the test has failed
	echo "$file" >> $tempFailedFile 
        failures=$((failures+1))
	if [[ $w != 0 ]]; then
	    newFailed=$((newFailed+1))
	fi
	echo -e "$numTests. $file [$failedString] [was $wasBefore]"
    else
	#the test has succeeded
	echo "$file" >> $tempPassedFile 
	if [[ $w != 2 ]]; then
	    newPassed=$((newPassed+1))
	fi
	echo -e "$numTests. $file [$passedString] [was $wasBefore]"
    fi
}

for FILE in `find ../TestCases/ -type f -name "*.js"`
do
    numTests=$((numTests+1))
    $P/ljs-test-single.sh $FILE $1
    testResult $FILE $?
done

failedList=`sort $tempFailedFile`
passedList=`sort $tempPassedFile`
echo "$failedList" > $failedFile
echo "$passedList" > $passedFile
passed=$((numTests-failures))
echo "$numTests tried. $passed passed ($newPassed new), $failures failed ($newFailed new)."
