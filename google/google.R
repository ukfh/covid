library(tidyverse)
library(tidyquant)
library(RCurl)
options(bitmapType='cairo')
source('common.R')

google.org <- getURL('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', timeout = 2000)
#read.csv(, header = T, stringsAsFactors = F)

#google.org <- read.csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', header = T, stringsAsFactors = F)
#google.org <- read.csv('google/2021-02-22_Global_Mobility_Report.csv',header = T, stringsAsFactors = F)
# save data ----
write.csv(google.org, file = paste('google/', Sys.Date(), '_Global_Mobility_Report.csv', sep = ''))
#google.org <- read.csv('google/2021-02-22_Global_Mobility_Report.csv', header = T, stringsAsFactors = F)
google.org <- google.org %>% mutate(date = as.Date(date)) # , dow = weekdays(date)
# google.org <- google.org %>% select(-dow)
google.long <- google.org %>% pivot_longer(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline)

# fish out the UK wide data
plotData <- google.long %>% filter( country_region_code == 'GB' & sub_region_1 =='')


uk <- plotData %>%  select (-census_fips_code,-metro_area,-iso_3166_2_code, -sub_region_2, -country_region,-country_region_code, -sub_region_1) %>%
#  filter(sub_region_1 %in% c('Reading', 'West Berkshire')) %>% 
  group_by(name) %>% arrange(date) %>% mutate (oldvalue = value) %>% 
  mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% 
  ungroup()
uk <- uk %>% mutate(dt = date)

# label the dates (Reading specific)
uk <- add_lockdown(uk)
# uk <- uk %>% mutate(dt = date, lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
#                                                                            dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
#                                                                            dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
#                                                                            dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
#                                                                            dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
#                                                                            dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
#                                                                            dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
#                                                                            dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
#                                                                            dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
#                                                                            dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))
# 
# uk$lockdown <- factor(uk$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))


# order the plots 

niceName  <- factor(c("Retail and \n Recreation","Grocery and \n Pharmacy" ,
                    "Parks","Transit","Workplace","Residential" ),
                  levels = c("Retail and \n Recreation","Grocery and \n Pharmacy" ,
                             "Parks","Transit","Workplace","Residential"  ))

name  <- factor(c("retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline" ,
                    "parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
                    "workplaces_percent_change_from_baseline","residential_percent_change_from_baseline" ),
                  levels = c("retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline" 
                             ,"parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
                             "workplaces_percent_change_from_baseline","residential_percent_change_from_baseline" ))
niceNames <- data.frame(name = name, niceName = niceName, stringsAsFactors = T)
uk <- merge(uk, niceNames, by = 'name')



file <- paste("google/movement_uk.png", sep = '')
gp <- ggplot(uk , aes(x=dt, y = value)) + geom_point(aes(colour=lockdown)) + 
  geom_line(aes(colour=lockdown)) + facet_grid(rows = vars(niceName), scales = 'free') + 
  ggtitle(paste("Google movement data for the UK ending ", max(uk$date), sep = '')) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
  theme_bw(base_size = 15) + ylab("Percent change from baseline") +  theme(axis.text.x=element_text(angle=60, hjust=1))
 ggsave(file, gp, width = 12, height = 9) 

 
# compare UK and Germany
 plotData <- google.long %>% filter( country_region_code %in% c('GB','DE') & sub_region_1 =='')
 
 
 plotData <- plotData %>%  select (-census_fips_code,-metro_area,-iso_3166_2_code, -sub_region_2, -country_region, -sub_region_1) %>%
   #  filter(sub_region_1 %in% c('Reading', 'West Berkshire')) %>% 
   group_by(name,country_region_code) %>% arrange(date) %>% mutate (oldvalue = value) %>% 
   mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% 
   ungroup()
# order the plots 
 
 niceName  <- factor(c("Retail and \n Recreation","Grocery and \n Pharmacy" ,
                       "Parks","Transit","Workplace","Residential" ),
                     levels = c("Retail and \n Recreation","Grocery and \n Pharmacy" ,
                                "Parks","Transit","Workplace","Residential"  ))
 
 name  <- factor(c("retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline" ,
                   "parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
                   "workplaces_percent_change_from_baseline","residential_percent_change_from_baseline" ),
                 levels = c("retail_and_recreation_percent_change_from_baseline","grocery_and_pharmacy_percent_change_from_baseline" 
                            ,"parks_percent_change_from_baseline","transit_stations_percent_change_from_baseline",
                            "workplaces_percent_change_from_baseline","residential_percent_change_from_baseline" ))
 niceNames <- data.frame(name = name, niceName = niceName, stringsAsFactors = T)
 plotData <- merge(plotData, niceNames, by = 'name')
 
 
 file <- paste("google/movement_uk_de.png", sep = '')
 gp <- ggplot(plotData , aes(x=date, y = value)) + geom_point(aes(colour=country_region_code)) + geom_line(aes(colour=country_region_code)) + 
   facet_grid(rows = vars(niceName), scales = 'free') + 
   ggtitle(paste("Google movement data for Germany and the UK ending ", max(uk$date), sep = '')) + 
   scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
   theme_bw(base_size = 15) + ylab("Percent change from baseline") +  theme(axis.text.x=element_text(angle=60, hjust=1))
 ggsave(file, gp, width = 12, height = 9)
 
 

# names <- sort(unique(uk$name))

# for(n in names)
# {
#   file <- paste("google/",n,".png", sep = '')
#   gp <- ggplot(uk %>% filter(name == n), aes(x=date, y = value)) + geom_point(aes(colour=lockdown)) + facet_grid(rows = vars(sub_region_1)) + ggtitle(paste(n, " ending ", max(uk$date), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
#   ggsave(file, gp, width = 12, height = 9)
# }


