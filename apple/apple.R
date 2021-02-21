# data downloaded from:
# https://www.apple.com/covid19/mobility
# https://covid19-static.cdn-apple.com/covid19-mobility-data/2102HotfixDev9/v3/en-us/applemobilitytrends-2021-02-20.csv

library(tidyverse)
library(tidyquant)

data <- read.csv('apple/applemobilitytrends-2021-02-17.csv', header = T, stringsAsFactors = F)

# turn this into a sane data format
data <- data %>% pivot_longer(-(geo_type:country))

# refromat the date
data <- data %>% mutate(dt = as.Date(name, "X%Y.%m.%d"), dow = weekdays(dt)) %>% select(-name) %>% as.data.frame()
str(data)
data <- data %>% filter(dt >= as.Date('2020-10-01'))
#View(unique(data$region))
#unique(data$alternative_name)

data.uk <- data %>% filter(region %in% c('United Kingdom'))
data.england <- data %>% filter(sub.region %in% c('England')) %>% select(dt,region,transportation_type,value)

#  smooth the england data with a rolling average
data.england.smooth <- data.england %>% group_by(region,transportation_type) %>% 
  arrange(dt) %>% mutate (oldvalue = value) %>%
  mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% ungroup()

# what transportation types are there?
t_type <- unique(data.england.smooth$transportation_type)

# label the dates (Reading specific)
data.england.smooth <- data.england.smooth %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                                           dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                                           dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                                           dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                                           dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                                           dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                                           dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                                           dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                                           dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                                           dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))

data.england.smooth$lockdown <- factor(data.england.smooth$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))

for(t in t_type)
{
  print(t)
  file <- paste('apple/england_', t, '.png', sep = '')
  plotData <- data.england.smooth %>% filter(transportation_type ==t)
  gp <- ggplot(plotData , aes(x=dt, y = value)) + geom_point(aes(colour=lockdown)) + facet_grid(rows = vars(region)) + ggtitle(t) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") #+ theme_bw(base_size = 20)
  
  ggsave(file, gp, width = 12, height = 9)
}


# how does this compare to Germany?
data.germany <- data  %>% filter(country %in% c('Germany') ) %>% filter(geo_type %in% c('city'))  %>% 
  select(dt,region,transportation_type,value)  %>% arrange(dt) %>% mutate (oldvalue = value) %>% as.data.frame()

data.germany <- data.germany %>%
  group_by(region,transportation_type) %>% arrange(dt) %>%
  mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% ungroup()
# what transportation types are there?
t_type <- unique(data.germany$transportation_type)

for(t in t_type)
{
  print(t)
  file <- paste('apple/germany_', t, '.png', sep = '')
  plotData <- data.germany %>% filter(transportation_type ==t)
  gp <- ggplot(plotData , aes(x=dt, y = value)) + geom_point() + facet_grid(rows = vars(region)) + ggtitle(t) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  
  ggsave(file, gp, width = 12, height = 9)
}




uk <- ggplot(data.uk %>% filter(region %in% c("England","Northern Ireland", "Scotland","Wales")), aes(x=dt, y = value)) + geom_point(aes(colour=transportation_type)) + facet_wrap(~region) 

ggsave('uk.png', uk, width = 12, height = 9)
