#!/bin/bash

# download and parse KML file
outDir=$1
mid=$2 # ID for midfile
outName=$3

lid="y84_ssX2HbU"

echo $outDir
echo $mid
echo $outName

mkdir -p ${outDir}/backup_old
mv ${outDir}/*kml ${outDir}/backup_old/.

#fetchUrl="https://www.google.com/maps/d/u/0/kml?mid=${mid}&cid=mp&forcekml=1"
fetchUrl="https://www.google.com/maps/d/u/0/kml?mid=${mid}&cid=mp&cv=53DpL5LnT98.en."

echo "Fetching map"
echo $fetchURL

wget "$fetchUrl"
wait
echo "got it!"

##dummy="kml?mid=${mid}&cid=mp&forcekml=1"
dummy="kml?mid=${mid}&cid=mp&cv=53DpL5LnT98.en."
mv $dummy ${outDir}/${outName}.kmz
###mv $dummy ${outDir}/${outName}.kml
curd=`pwd`
cd $outDir
unzip ${outDir}/${outName}.kmz
mv ${outDir}/doc.kml ${outName}.kml
rm -r ${outDir}/images
cd $curd
# strip the CDATA tags that prevent some names from registering
# in the parser
perl -pe 's/<\!\[CDATA\[(.*)\]\]>/$1/' ${outDir}/${outName}.kml | perl -pe 's/<\!\[CDATA\[(.*)/$1/' | perl -pe 's/^\]\]//'> ${outDir}/${outName}_clean.kml

perl -pe 's/<description>(.*)<\/description>//' ${outDir}/${outName}_clean.kml > ${outDir}/${outName}_clean2.kml
perl -pe 's/&/&amp;/g' ${outDir}/${outName}_clean2.kml > ${outDir}/${outName}_clean3.kml

# make table
Rscript kml_parse_extended.R ${outDir}/${outName}_clean3.kml

#rm ${outDir}/${outName}_clean.kml
#rm ${outDir}/${outName}_clean2.kml
#rm ${outDir}/${outName}_clean3.kml

exit 0;

