#!/bin/bash

# download and parse KML file
outDir=$1
mid=$2 # ID for midfile
outName=$3

echo $outDir
echo $mid
echo $outName

fetchUrl="https://www.google.com/maps/d/u/0/kml?mid=${mid}&forcekml=1"
wget "$fetchUrl"
wait

dummy="kml?mid=${mid}&forcekml=1"
mv $dummy ${outDir}/${outName}.kml
# strip the CDATA tags that prevent some names from registering
# in the parser
perl -pe 's/<\!\[CDATA\[(.*)\]\]>/$1/' ${outDir}/${outName}.kml > ${outDir}/${outName}_clean.kml

perl -pe 's/<description>(.*)<\/description>//' ${outDir}/${outName}_clean.kml > ${outDir}/${outName}_clean2.kml
perl -pe 's/&/&amp;/g' ${outDir}/${outName}_clean2.kml > ${outDir}/${outName}_clean3.kml

# make table
Rscript kml_parse_extended.R ${outDir}/${outName}_clean3.kml

#rm ${outDir}/${outName}_clean.kml
#rm ${outDir}/${outName}_clean2.kml
#rm ${outDir}/${outName}_clean3.kml

exit 0;

