#!/bin/bash

# auto get new cases from GTA 

# findNewRecords.R - compares current to last snapshot (1700 EDT daily) and
# collects records with differences.
#
# annotNewRecords.R - converts the new records into the csv format needed
# for the Google map. Uses Ontario school lat/long info to auto-populate
# those fields and adds entries for other fields found in Google map
# 
# Note on args: Each school has two args associated with it:
# 1) the folder name (no spaces) : e.g. "Toronto_DSB"
# 2) name of school board: must match school board name in Ontario-wide
# list of lat/long


#Rscript findNewRecords.R Toronto_DSB
#Rscript annotNewRecords.R Toronto_DSB "Toronto DSB"


#Rscript findNewRecords.R Peel_DSB
#Rscript annotNewRecords.R Peel_DSB "Peel DSB"

#Rscript findNewRecords.R HDSB
#Rscript annotNewRecords.R HDSB "Halton DSB"

#Rscript findNewRecords.R HCDSB
#Rscript annotNewRecords.R HCDSB "Halton CDSB"

#Rscript findNewRecords.R DDSB
#Rscript annotNewRecords.R DDSB "Durham DSB"

#Rscript findNewRecords.R DPCDSB
#Rscript annotNewRecords.R DPCDSB "Dufferin-Peel CDSB"

#Rscript findNewRecords.R YRDSB
#Rscript annotNewRecords.R YRDSB "York Regional DSB"



