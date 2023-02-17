#!/bin/bash

# Mapbox-Google sheet version of nightly-build
# Pulls Google map for @covidschoolsCA and @CovidEcoles, parses and 
# converts into tables for downstream use.

#outRoot=/home/shraddhapai/Canada_COVID_tracker
outRoot=/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker
qcDir=/Users/shraddhapai/Google_covidschools/daily_data/QC

# Quebec tracker
QCMidFile=1S-b-tmhKP1RQeMaIZslrR_hqApM-KERq
QCMidFile2="1S-b-tmhKP1RQeMaIZslrR_hqApM-KERq&lid=I0FmFF_CM9A"

TZ="America/Toronto"
dt=`date +%y%m%d`
echo "Date is $dt"
outDir=${outRoot}/export-${dt}
mkdir -p $outDir

logfile=${outDir}/nightly-build.log
touch $logfile

echo "******************************************************"
echo "Fetching Google sheet data"
echo "******************************************************"
Rscript collectData.R $outDir $dt


echo "******************************************************"
echo "Fetching Quebec map"
echo "******************************************************"
./setup_datatable_quebec2.sh $outDir $QCMidFile COVIDEcolesQuebec 

echo "******************************************************"
echo "Fetching Quebec map layer 2"
echo "******************************************************"
./setup_datatable_quebec2.sh $outDir $QCMidFile2 COVIDEcolesQuebec_layer2 


echo "******************************************************" >> $logfile
echo " Merging" >> $logfile
###echo "******************************************************" >> $logfile
Rscript mergeQC.R $dt ${outRoot}/export >> $logfile

echo "******************************************************" >> $logfile
echo " Fetching CEQ annotation sheet" >> $logfile
echo "******************************************************" >> $logfile
Rscript fetchQCstats.R ${outRoot}/export
echo "Cleaning" >> $logfile
echo $outRoot
Rscript qcStats.R ${outRoot}/export
cp ${outRoot}/export-${dt}/CEQ_annotated_clean*csv ${qcDir}/.

echo "******************************************************" >> $logfile
echo " Final cleanup " >> $logfile
echo "******************************************************" >> $logfile

# This script does the heavy lifting of cleaning the entered data, and frequently paused when assertions failed. It is best run manually.
Rscript cleanMapData.R >> $logfile

# These scripts are best run manually too.
#### Call makePlots.R and schoolBoard.R after this.
###
