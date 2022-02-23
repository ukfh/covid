# data downloaded from:
# https://www.apple.com/covid19/mobility
# https://covid19-static.cdn-apple.com/covid19-mobility-data/2102HotfixDev9/v3/en-us/applemobilitytrends-2021-02-20.csv

library(tidyverse)
library(tidyquant)
source('common.R')

data <- read.csv('apple/applemobilitytrends-2022-02-19.csv', header = T, stringsAsFactors = F)

# turn this into a sane data format
data <- data %>% pivot_longer(-(geo_type:country))

# reformat the date
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


data.uk.smooth <- add_lockdown(data.uk.smooth)


file <- paste('apple/uk.png', sep = '')
plotData_t <- data.uk.smooth
gp <- ggplot(plotData_t , aes(x=dt, y = value)) + geom_point(aes(colour=lockdown)) + geom_line(aes(colour=lockdown))  + facet_grid(rows = vars(transportation_type),scales = 'free') + ggtitle(paste("Apple UK movement data until ", max(plotData_t$dt), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15) +  theme(axis.text.x=element_text(angle=60, hjust=1))
    
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
gp <- ggplot(plotData_t , aes(x=dt, y = value)) + geom_point(aes(colour=region)) + geom_line(aes(colour=region))  + facet_grid(rows = vars(transportation_type),scales = 'free') + ggtitle(paste("Apple UK & DE movement data until ", max(plotData_t$dt), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + theme_bw(base_size = 15) +  theme(axis.text.x=element_text(angle=60, hjust=1))

ggsave(file, gp, width = 15, height = 9)

