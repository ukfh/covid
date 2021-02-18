library(httr)
library(jsonlite)
library(tidyverse)
library(tidyquant)

 # get data ====

source('gov/gov_uk.R')


# tidy the data-----
# 

# add all deaths ----
deaths.all <- deaths %>% group_by(dt) %>% summarise(deaths = sum(deaths)) %>% ungroup() %>% mutate(country = 'all')
deaths <- bind_rows(deaths, deaths.all)
rm(deaths.all)
deaths <- deaths %>% mutate(metric = 'deaths') 
deaths <- deaths %>% mutate(value = deaths) %>% select(-deaths)

# new cases -----
# 
new_cases <- new_cases %>% mutate(country = 'all') 
new_cases <- new_cases %>% gather(key = metric, value, -dt, -country)
new_cases <- new_cases %>% mutate(metric = 'new_cases')
# hospital cases ----
# 
hospitalCases <- hospitalCases %>% mutate(country = 'all') 
hospitalCases <- hospitalCases %>% gather(key = metric, value, -dt, -country)
hospitalCases <- hospitalCases %>% mutate(metric = 'hospital_cases')

# tests ----
tests <- tests %>% mutate(country = 'all') %>% gather(key = metric, value, -dt, -country)

# test2 ----
tests2 <- tests2 %>% gather(key = metric, value, -dt) %>% mutate(country = 'all')
tests2 <- tests2 %>% select(dt, country, everything())
tests2.all <- tests2 %>% group_by(dt, country) %>% summarise(value = sum(value, na.rm = T)) %>% ungroup() %>% mutate(metric = 'test_p1_p4')
tests2.all <- tests2.all %>% select(dt, country,metric,value)
tests2 <- bind_rows(tests2,tests2.all)
rm(tests2.all)

corona <- bind_rows(deaths, new_cases, hospitalCases, tests, tests2)
# save the raw data ----
file <- paste('gov/', Sys.Date(), "_gov_covid_raw.csv", sep = '')
write.csv(corona, file = file, row.names = F)

rm(list = (c('data','deaths', 'tests', 'tests2', 'new_cases','response', 'patients_in_hospital', 'hospitalCases')))

# some contortions to add this up for the entire UK ----
corona_wide <- corona %>% spread(metric, value)
corona_wide <- corona_wide %>% mutate(new_cases_per_test = round(new_cases / test_p1_p4 , digits = 3))
corona_wide <- corona_wide %>% mutate(deaths_per_hopital = round(deaths / hospital_cases , digits =  3))

corona_wide_all <- corona_wide %>% filter(country == 'all')

# we're just interested in all of the UK ----
# 
corona_long <- corona_wide_all %>% filter(country == 'all') %>% select(-country) %>% gather(key, value, -dt)
corona_long <- corona_long %>% arrange(key, dt) %>% group_by(key) %>% 
  mutate(rolling_avg = round(rollmean(value,7, na.pad = T), digits = 1) ,
         rolling_avg_14 = round(rollmean(value,14, na.pad = T), digits = 1) ,
         seven_days = round(rollapply(value, 7, sum, fill = T, align = 'left') / 667, digits = 1), # 667 is an estimate of the population of the UK
         daily = round(value / 680, digits = 1),
         seven_days_total = rollapply(value, 7, sum, fill = T, align = 'left') ) %>% ungroup()

# categoise the timeline
corona_long <- corona_long %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                                       dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                                       dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                                       dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                                       dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                                       dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                                       dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                                       dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                                       dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                                       dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))

corona_long$lockdown <- factor(corona_long$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal',
                                                                'Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))

# unique(corona_long$lockdown)

plotData <- corona_long %>% filter(key  %in% c("deaths","hospital_cases","new_cases" ) & dt >= as.Date('2020-10-15'))
gp <- ggplot(plotData, aes(x= dt, y = rolling_avg, colour = lockdown)) + geom_point(aes(colour = lockdown)) + geom_line(aes(colour = lockdown)) + 
  # facet_wrap(~key, scales = 'free') + scale_y_log10() + xlab('Time') + ylab('7 day rolling average')  + 
  facet_grid(rows=vars(key), scales = 'free') + scale_y_log10() + 
  xlab('Time') + ylab('7 day rolling average \n with daily figures as thin line')  + 
  geom_line(aes(x=dt, y=value, color=lockdown),  lwd = 0.75) + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + ggtitle(paste('Covid figures on ', max(plotData$dt), sep = '')) + theme_bw(base_size = 20)
ggsave(gp,filename = 'gov/uk_covid_log.png', height = 6, width = 12)

  
gp <- ggplot(plotData , aes(x= dt, y = rolling_avg, colour = lockdown)) + geom_point(aes(colour = lockdown)) + geom_line(aes(colour = lockdown)) +   # facet_wrap(~key, scales = 'free') + scale_y_log10() + xlab('Time') + ylab('7 day rolling average')  + 
  facet_grid(rows=vars(key), scales = 'free')  + xlab('Time') + ylab('7 day rolling average \n with daily figures as thin line')   + 
  geom_line(aes(x=dt, y=value, color=lockdown),  lwd = 0.75)  + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + ggtitle(paste('Covid figures on ', max(plotData$dt), sep = '')) + theme_bw(base_size = 20)
ggsave(gp,filename = 'gov/uk_covid_lin.png', height = 6, width = 12)


