#!/bin/bash

inDir=/Users/shraddhapai/Google_covidschools/SchoolBoard_daily_snapshot

dirList=`ls $inDir`
for k in $dirList; do
	fulldir="${inDir}/${k}"
	ct=`ls ${fulldir}/*.* | wc -l`
	echo -e "$k\t$ct"

done
