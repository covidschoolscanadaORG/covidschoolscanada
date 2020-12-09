
#inDir=/Users/shraddhapai/Google_covidschools/daily_data/Canada_COVID_tracker/10-2020
inDir=/Users/shraddhapai/Google_covidschools/11-2020

for x in ${inDir}/export-*; do
	echo $x
	# set to ro all source and final data files
	chmod u-w ${x}/CanadaMap.kml
	chmod u-w ${x}/COVIDEcolesQuebec.kml
	chmod u-w ${x}/*.pdf
	chmod u-w ${x}/*clean.csv
	chmod u-w ${x}/final_data/*clean.csv

	# delete large png and jpg files
	# delete intermediate kml
	rm ${x}/*.png 
	rm ${x}/*.png
	rm ${x}/social_media/*.png
	rm ${x}/social_media/*.jpg
	rm ${x}/CanadaMap_clean*kml
	rm ${x}/COVIDEcolesQuebec_clean*kml
	rm ${x}/COVIDEcolesQuebec_clean3.kml-*txt
	rm -r ${x}/backup_old
	#ls ${x}/COVIDEcolesQuebec_clean*kml
done
