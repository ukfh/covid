# Covid

Various scripts to plot COVID numbers.

## Our World in Data
The [data](https://covid.ourworldindata.org/data/owid-covid-data.csv) originates from the [Our World in Data](https://covid.ourworldindata.org/).

![The UK](owid/gbr.png?raw=true "The UK")
![The UK compared to Germany](owid/gbr-de.png?raw=true "The UK compared to Germany")
![The vaccine race between the UK and Germany](owid/vaccine_race.png?raw=true "The vaccine race between the UK and Germany")

It's also interesting to look at the numbers for the UK with logarithmic y-axis. Periods which can be approximated by straight lines indicate exponential growth or decline.
![The UK](owid/gbr_log.png?raw=true "The UK")

## UK government
Data from the [UK Government](https://coronavirus.data.gov.uk/details/developers-guide). The fatter line is the 7 day moving average and the thinner line are the actual daily values. The data is plotted with a linear and a logarithimc y-axis. A simple regression assuming exponential growth based on the last few weeks (see code) has been added. New cases are for England only.

![UK linear plot](gov/uk_covid_lin.png?raw=true "UK linear plot")
![UK logarithmic plot](gov/uk_covid_log.png?raw=true "UK logarithmic plot")

## Google Mobilty data
Data provided by [Google](https://www.google.com/covid19/mobility/). This is a 7 day rolling average.

![Mobililty UK](google/movement_uk.png?raw=true "Mobility UK")
![Mobility UK and Germany](google/movement_uk_de.png?raw=true "Mobility UK and Germany")

## Apple Mobility data
Apple no longer provides this data and the plot below is a static display.
Data from [Apple](https://covid19.apple.com/mobility). This is again a 7 day rolling average. The missing data is caused by missing raw data that upsets the rolling average. 

![Mobility UK](apple/uk.png?raw=true "Mobility UK")
![Mobility UK and Germany](apple/uk_de.png?raw=true "Mobility UK and Germany")



