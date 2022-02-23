MAILTO="uliharder@gmail.com"
MAILFROM="uliharder@gmail.com"
# covid
30 16,17,18 * * * /home/uh/R/covid/gov.sh
30 9 * * *  /home/uh/R/covid/owid.sh
30 11 * * *    /home/uh/R/covid/google.sh

# weather data
15 1,5,9,13,17,21 * * * /home/uh/weather/weather/pipeline.sh
