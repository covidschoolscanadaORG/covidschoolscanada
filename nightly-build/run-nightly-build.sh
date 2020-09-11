#!/bin/bash

outRoot=~/Canada_COVID_tracker

# Masks4Canada Canada-wide school tracker
canadaMidFile=1blA_H3Hv5S9Ii_vyudgDk-j6SfJQil9S
# Quebec tracker
QCMidFile=1S-b-tmhKP1RQeMaIZslrR_hqApM-KERq

dt=`date +%Y%m%d`
outDir=${outRoot}/export-${dt}
mkdir -p $outDir

./setup_datatable.sh $outDir $canadaMidFile CanadaMap
#./setup_datatable_quebec.sh $outDir $QCMidFile COVIDEcolesQuebec

