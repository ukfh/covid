#!/bin/bash

DIR=/home/uh/R/covid
DATE=`date`

/usr/local/bin/Rscript gov/corona.R 

git add gov/uk_covid_l*
git commit -m "plot update on $DATE"
git push origin main

exit
