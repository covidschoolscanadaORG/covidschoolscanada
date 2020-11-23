#!/bin/bash

# Pulls Google map for @covidschoolsCA and @CovidEcoles, parses and 
# converts into tables for downstream use.

#outRoot=/home/shraddhapai/Canada_COVID_tracker
outRoot=/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker

# Masks4Canada Canada-wide school tracker
canadaMidFile=1blA_H3Hv5S9Ii_vyudgDk-j6SfJQil9S
# Quebec tracker
QCMidFile=1S-b-tmhKP1RQeMaIZslrR_hqApM-KERq

TZ="America/Toronto"
dt=`date +%y%m%d`
echo "Date is $dt"
outDir=${outRoot}/export-${dt}
mkdir -p $outDir

logfile=${outDir}/nightly-build.log
touch $logfile

echo "******************************************************"
echo "Fetching Canada-wide map"
echo "******************************************************"
./setup_datatable.sh $outDir $canadaMidFile CanadaMap > $logfile

echo "******************************************************"
echo "Fetching Quebec map"
echo "******************************************************"
./setup_datatable_quebec2.sh $outDir $QCMidFile COVIDEcolesQuebec 

exit 0

echo "******************************************************" >> $logfile
echo " Merging" >> $logfile
echo "******************************************************" >> $logfile
Rscript mergeQC.R $dt ${outRoot}/export >> $logfile

echo "******************************************************" >> $logfile
echo " Fetching CEQ annotation sheet" >> $logfile
echo "******************************************************" >> $logfile
Rscript fetchQCstats.R ${outRoot}/export
echo "Cleaning" >> $logfile
Rscript qcStats.R ${outRoot}/export

###echo "******************************************************" >> $logfile
###echo " Fetch auto-generated entries " >> $logfile
###echo "******************************************************" >> $logfile
###dt2=$(date +%Y-%m-%d -d "$(date) - 1 day")
####dt2=$(date +%Y-%m-%d)
###baseURL=https://covidschoolboards.s3.ca-central-1.amazonaws.com
###tgtDir=/home/shraddhapai/Canada_COVID_tracker/AutoGen
###inFile=${baseURL}/Automated_boards_${dt2}.csv
###wget $inFile
###mv Automated_*.csv ${tgtDir}/.

echo "******************************************************" >> $logfile
echo " Final cleanup " >> $logfile
echo "******************************************************" >> $logfile
#Rscript cleanMapData.R >> $logfile
#### Call makePlots.R and schoolBoard.R after this.
###
