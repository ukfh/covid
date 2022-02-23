library(tidyverse)
source('common.R')
# get the OWID data
url <- 'https://covid.ourworldindata.org/data/owid-covid-data.csv'
options(bitmapType='cairo')

data.org <- read.csv(url, header = T, stringsAsFactors = F)

# save data ----
write.csv(data.org, file = paste('owid/', Sys.Date(), '_owid-covid-data.csv', sep = ''))

# tidy the data
data.long <- data.org %>% gather(key = metric, value, -(1:4)) %>% mutate(date = as.Date(date), dt= as.Date(date), value = as.numeric(value))

# look at he UK first
gbr <- data.long %>% filter(iso_code == 'GBR')
# unique(data.long$metric)

gbr <- add_lockdown(gbr)


  
# gbr plot -----
file <- paste('owid/gbr.png', sep = '')
plotData <- gbr %>% filter(metric %in% c("new_cases","new_cases_smoothed","positive_rate" ,"new_deaths_smoothed"  ,"icu_patients" ))
# order the plot
niceName <- factor(c("New cases","New cases \n smoothed","ICU patients" ,"New deaths \n smoothed" ,"Positive rate"  ),
                   levels =c("New cases","New cases \n smoothed","ICU patients" ,"New deaths \n smoothed" ,"Positive rate"  ))
metric  <- factor(c("new_cases","new_cases_smoothed","icu_patients" ,"new_deaths_smoothed","positive_rate" ),
                  levels = c("new_cases","new_cases_smoothed","icu_patients" ,"new_deaths_smoothed","positive_rate" ))
niceNames <- data.frame(metric = metric, niceName = niceName, stringsAsFactors = T)
plotData <- merge(plotData, niceNames, by = 'metric')
gbrPlot <-  ggplot(plotData, aes(x= date, y = value)) + 
  geom_point(aes(colour=lockdown)) + geom_line() + facet_grid(rows=vars(niceName), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15) + ggtitle(paste("GBR data ending ", max(plotData$date), sep = "")) +  theme(axis.text.x=element_text(angle=60, hjust=1))

ggsave(file, gbrPlot, width = 12, height = 9)

# log plot ----
file <- paste('owid/gbr_log.png', sep = '')
gbrPlot <-  ggplot(plotData %>% filter(date >= as.Date('2020-04-01') & niceName != 'Positive rate'), aes(x= date, y = value)) + 
  geom_point(aes(colour=lockdown)) + geom_line() + facet_grid(rows=vars(niceName), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15) + ggtitle(paste("GBR data ending ", max(plotData$date), sep = ""))  + scale_y_log10() + 
  theme(axis.text.x=element_text(angle=60, hjust=1))

ggsave(file, gbrPlot, width = 12, height = 9)



# comapre countries ----
# country comparison other country codes ,'DNK','NLD'

plotData <- data.long %>% filter(iso_code %in% c('GBR','DEU')) %>% filter(metric %in% c("new_cases","new_cases_smoothed","positive_rate" ,"new_deaths_smoothed"  ,"icu_patients" ,"new_cases_smoothed_per_million"))
# order the plots 
niceName <- factor(c("New cases","New cases \n smoothed", "New cases \n smoothed per \n million","ICU patients" ,"New deaths \n smoothed" ,"Positive rate"  ),
                   levels =c("New cases","New cases \n smoothed", "New cases \n smoothed per \n million","ICU patients" ,"New deaths \n smoothed" ,"Positive rate"  ))
metric  <- factor(c("new_cases","new_cases_smoothed", "new_cases_smoothed_per_million","icu_patients" ,"new_deaths_smoothed","positive_rate" ),
                  levels = c("new_cases","new_cases_smoothed","new_cases_smoothed_per_million","icu_patients" ,"new_deaths_smoothed","positive_rate" ))
niceNames <- data.frame(metric = metric, niceName = niceName, stringsAsFactors = T)
plotData <- merge(plotData, niceNames, by = 'metric')

countryData <- ggplot(plotData , aes(x= date, y = value, colour = iso_code)) + geom_point(aes(colour = iso_code)) + geom_line(aes(colour = iso_code)) + facet_grid(rows=vars(niceName), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15) + 
  ggtitle(paste("Comparing ", paste(unique(plotData$iso_code), collapse = " "), " data ending ", max(plotData$date), sep = "")) +  theme(axis.text.x=element_text(angle=60, hjust=1))

file <- paste('owid/gbr-de.png', sep = '')
ggsave(file, countryData, width = 12, height = 9)  

# vaccination comparison
# 
plotData <- data.long %>% filter(iso_code %in% c('GBR','DEU')) %>% 
  filter(metric %in% c('people_fully_vaccinated',"total_vaccinations" ) & date >= as.Date('2020-12-01'))
# order the plots 
niceName <-  factor(c('People fully vaccinated',"Total vaccinations" ),
                    levels = c('People fully vaccinated',"Total vaccinations" ))
metric  <- factor(c('people_fully_vaccinated',"total_vaccinations" ),
                  levels = c('people_fully_vaccinated',"total_vaccinations" ))
niceNames <- data.frame(metric = metric, niceName = niceName, stringsAsFactors = T)
plotData <- merge(plotData, niceNames, by = 'metric')

countryData <- ggplot(plotData, aes(x= date, y = value, colour = iso_code)) + geom_point(aes(colour = iso_code)) + geom_line(aes(colour = iso_code)) + facet_grid(rows=vars(niceName), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 20) + 
  ggtitle(paste("Comparing vaccinations in ", paste(unique(plotData$iso_code), collapse = " "), " data ending ", max(plotData$date), sep = "")) +  theme(axis.text.x=element_text(angle=60, hjust=1))
  

file <- paste('owid/vaccine_race.png', sep = '')
ggsave(file, countryData, width = 12, height = 9)  

