#!/bin/bash

DIR=/home/uh/R/covid
DATE=`date`

cd $DIR
/usr/local/bin/Rscript gov/corona_v3.R 

git add gov/uk_covid_l*
git commit -m "plot update on $DATE"
git push origin main

exit
