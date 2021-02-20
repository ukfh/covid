library(tidyverse)
  

url <- 'https://covid.ourworldindata.org/data/owid-covid-data.csv'
  
data.org <- read.csv(url, header = T, stringsAsFactors = F)

# save data ----
write.csv(data.org, file = paste('owid/', Sys.Date(), '_owid-covid-data.csv', sep = ''))
  
data.long <- data.org %>% gather(key = metric, value, -(1:4)) %>% mutate(date = as.Date(date), value = as.numeric(value))
  
gbr <- data.long %>% filter(iso_code == 'GBR')
# unique(data.long$metric)

gbr <- gbr %>% mutate(dt = date, 
                      lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                           dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                           dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                           dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                           dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                           dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                           dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                           dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                           dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                           dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))

gbr$lockdown <- factor(gbr$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))

  
# "new_deaths"
file <- paste('owid/gbr.png', sep = '')
gbrPlot <-  ggplot(gbr %>% filter(metric %in% c("new_cases","new_cases_smoothed","positive_rate" ,"new_deaths_smoothed"  ,"icu_patients" )), aes(x= date, y = value)) + 
  geom_point(aes(colour=lockdown)) + geom_line() + facet_grid(rows=vars(metric), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 20)

ggsave(file, gbrPlot, width = 12, height = 9)


# country comparison # ,'DNK','NLD'
data <- data.long %>% filter(iso_code %in% c('GBR','DEU'))
  # unique(data.long$metric)

raceData <- ggplot(data %>% filter(metric %in% c("new_cases_smoothed","positive_rate" ,"new_deaths_smoothed"  ,"icu_patients" )) , aes(x= date, y = value, colour = iso_code)) + geom_point(aes(colour = iso_code)) + geom_line(aes(colour = iso_code)) + facet_grid(rows=vars(metric), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 20)

file <- paste('owid/gbr-de.png', sep = '')
ggsave(file, raceData, width = 12, height = 9)  
  
raceData <- ggplot(data %>% filter(date >= as.Date('2020-12-15') & metric %in% c('people_fully_vaccinated',"total_vaccinations" )), aes(x= date, y = value, colour = iso_code)) + geom_point(aes(colour = iso_code)) + geom_line(aes(colour = iso_code)) + facet_grid(rows=vars(metric), scales = 'free') + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 20) + ggtitle(paste("Vaccinations ending ",max(data$date), sep = ''))

file <- paste('owid/vaccine_race.png', sep = '')
ggsave(file, raceData, width = 12, height = 9)  

