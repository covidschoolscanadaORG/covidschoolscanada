#!/bin/bash

# download AB autogen. move to appropriate folder
#dt=`gdate +%Y-%m-%d -d "yesterday"` 
#dt=`gdate +%Y-%m-%d -d "yesterday"` 
dt=`gdate +%Y-%m-%d`
#dt=2021-04-09
baseURL=https://covidschoolboards.s3.ca-central-1.amazonaws.com/AB_Automated_boards_${dt}.csv
abDir=/Users/shraddhapai/Google_covidschools/daily_data/AB
echo "AB autogen: fetching $dt"
wget $baseURL
baseF=`basename $baseURL`
echo "Moving"
mv $baseF ${abDir}/.

# download ON autogen. replace google spreadsheet
baseURL=https://covidschoolboards.s3.ca-central-1.amazonaws.com/Automated_boards_${dt}.csv
echo "ON autogen: fetching $dt"
wget $baseURL
echo "Move to autogen folder"
onDir=/Users/shraddhapai/Google_covidschools/daily_data/ON
baseF=`basename $baseURL`
mv ${baseF} ${onDir}/.

