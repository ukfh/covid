#!/bin/bash

DIR=/home/uh/R/covid

Rscript gov/corona.R 

git add gov/uk_covid_l*
git commit -m "plot update"
git push origin main

exit
