# data downloaded from:
# https://www.apple.com/covid19/mobility
# https://covid19-static.cdn-apple.com/covid19-mobility-data/2102HotfixDev9/v3/en-us/applemobilitytrends-2021-02-20.csv

library(tidyverse)
library(tidyquant)

data <- read.csv('apple/applemobilitytrends-2021-03-03.csv', header = T, stringsAsFactors = F)

# turn this into a sane data format
data <- data %>% pivot_longer(-(geo_type:country))

# refromat the date
data <- data %>% mutate(dt = as.Date(name, "X%Y.%m.%d"), dow = weekdays(dt)) %>% select(-name) %>% as.data.frame()
#str(data)
# data <- data %>% filter(dt >= as.Date('2020-10-01'))
#View(unique(data$region))
#unique(data$alternative_name)

data.uk <- data %>% filter(region %in% c('United Kingdom')) %>% filter(geo_type == 'country/region') %>% select(dt,region,transportation_type,value)

#  smooth the uk data with a rolling average
data.uk.smooth <- data.uk %>% group_by(region,transportation_type) %>% 
  arrange(dt) %>% mutate (oldvalue = value) %>%
  mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% ungroup()



# label the dates (Reading specific)
data.uk.smooth <- data.uk.smooth %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                                           dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                                           dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                                           dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                                           dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                                           dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                                           dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                                           dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                                           dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                                           dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))

data.uk.smooth$lockdown <- factor(data.uk.smooth$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))


file <- paste('apple/uk.png', sep = '')
plotData_t <- data.uk.smooth
gp <- ggplot(plotData_t , aes(x=dt, y = value)) + geom_point(aes(colour=lockdown)) + geom_line(aes(colour=lockdown))  + facet_grid(rows = vars(transportation_type),scales = 'free') + ggtitle(paste("Apple UK movement data until ", max(plotData_t$dt), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15)
    
ggsave(file, gp, width = 15, height = 9)


# compare Uk & Germany -----
# 
data.comp <- data %>% filter(region %in% c('United Kingdom','Germany')) %>% filter(geo_type == 'country/region') %>% select(dt,region,transportation_type,value)

#  smooth the uk data with a rolling average
data.comp.smooth <- data.comp %>% group_by(region,transportation_type) %>% 
  arrange(dt) %>% mutate (oldvalue = value) %>%
  mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% ungroup()


file <- paste('apple/uk_de.png', sep = '')
plotData_t <- data.comp.smooth
gp <- ggplot(plotData_t , aes(x=dt, y = value)) + geom_point(aes(colour=region)) + geom_line(aes(colour=region))  + facet_grid(rows = vars(transportation_type),scales = 'free') + ggtitle(paste("Apple UK & DE movement data until ", max(plotData_t$dt), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15)

ggsave(file, gp, width = 15, height = 9)

