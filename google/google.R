library(tidyverse)
library(tidyquant)

google.org <- read.csv('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', header = T, stringsAsFactors = F)
# save data ----
write.csv(data.org, file = paste('google/', Sys.Date(), '_Global_Mobility_Report.csv', sep = ''))
google.org <- google.org %>% mutate(date = as.Date(date)) # , dow = weekdays(date)
# google.org <- google.org %>% select(-dow)
google.long <- google.org %>% pivot_longer(retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline)

# google.long.smooth <- google.long %>% group_by(country_region_code,country_region,sub_region_1,sub_region_2,metro_area,iso_3166_2_code,census_fips_code) %>% 
#   arrange(date) %>% mutate (oldvalue = value) %>%
#   mutate ( value = round(rollmean(value,7, na.pad = T), digits = 1)) %>% ungroup()
# 
# 
# unique(google.org$country_region_code)

uk <- google.long %>% filter(country_region_code == 'GB') %>% select (-census_fips_code,-metro_area,-iso_3166_2_code, -sub_region_2, -country_region,-country_region_code) %>%
  filter(sub_region_1 %in% c('Reading', 'West Berkshire',"County Durham","Nottingham")) %>% group_by(sub_region_1,name) %>%   
  arrange(date) %>% mutate (oldvalue = value) %>% mutate ( value = round(rollmean(value,5, na.pad = T), digits = 1)) %>% ungroup()

# label the dates (Reading specific)
uk <- uk %>% mutate(dt = date, lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                                                           dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                                                           dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                                                           dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                                                           dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                                                           dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                                                           dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                                                           dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                                                           dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                                                           dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))

uk$lockdown <- factor(uk$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))



names <- sort(unique(uk$name))

for(n in names)
{
  file <- paste("google/",n,".png", sep = '')
  gp <- ggplot(uk %>% filter(name == n), aes(x=date, y = value)) + geom_point(aes(colour=lockdown)) + facet_grid(rows = vars(sub_region_1)) + ggtitle(paste(n, " ending ", max(uk$date), sep = '')) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
  ggsave(file, gp, width = 12, height = 9)
}


