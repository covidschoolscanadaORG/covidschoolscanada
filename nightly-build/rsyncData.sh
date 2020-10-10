# run on SP machine

srcDir=/home/shraddhapai/Canada_COVID_tracker
tgtDir="/Users/shraddhapai/Google_covidschools/daily_data"
rsync -ravz shraddhapai@192.168.81.205:${srcDir} $tgtDir
