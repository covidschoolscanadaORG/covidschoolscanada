# fetch auto-gen table
dt2=$(date +%Y-%m-%d -d "$(date) - 1 day")
#dt2=$(date +%Y-%m-%d)-1
echo $dt2
baseURL=https://covidschoolboards.s3.ca-central-1.amazonaws.com
tgtDir=/home/shraddhapai/Canada_COVID_tracker/AutoGen
inFile=${baseURL}/Peel_${dt2}.csv
echo $inFile
wget $inFile
baseF=`basename $inFile`
mv $baseF ${tgtDir}/.
chmod u-w ${tgtDir}/${baseF}
