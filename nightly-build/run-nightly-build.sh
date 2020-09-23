#!/bin/bash

outRoot=~/Canada_COVID_tracker

# Masks4Canada Canada-wide school tracker
canadaMidFile=1blA_H3Hv5S9Ii_vyudgDk-j6SfJQil9S
# Quebec tracker
QCMidFile=1S-b-tmhKP1RQeMaIZslrR_hqApM-KERq
#1blA_H3Hv5S9Ii_vyudgDk-j6SfJQil9S

dt=`date +%y%m%d`
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
./setup_datatable_quebec.sh $outDir $QCMidFile COVIDEcolesQuebec >> $logfile

echo "******************************************************" >> $logfile
echo " Merging" >> $logfile
echo "******************************************************" >> $logfile
Rscript mergeQC.R $dt >> $logfile
Rscript cleanMapData.R >> $logfile

######echo "******************************************************"
######echo " Making plots"
######echo "******************************************************"
####Rscript makePlots.R >> $logfile
######
######echo "Tasks completed."
######echo `date`
######
