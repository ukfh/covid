#!/bin/bash

DIR=/home/uh/R/covid
DATE=`date`

Rscript owid/ourworldindata.R 

git add owid/*.png
git commit -m "plot update on $DATE"
git push origin main

exit
