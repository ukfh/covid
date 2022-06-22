#!/bin/bash

DIR=/home/uh/R/covid
DATE=`date`

cd $DIR
/usr/local/bin/Rscript google/google.R 

git add google/move*.png
git commit -m "Google plot update on $DATE"
git push origin main

exit
