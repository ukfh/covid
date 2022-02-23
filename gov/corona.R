library(httr)
library(jsonlite)
library(tidyverse)
library(tidyquant)
options(bitmapType='cairo')
source('common.R')

# get data -------

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

# categorise the timeline
corona_long <- add_lockdown(corona_long)

plotData <- corona_long %>% filter(key  %in% c("deaths","hospital_cases","new_cases" ) & dt >= Sys.Date() - 12 * 7 ) #as.Date('2020-04-01'))

# order the plot
niceName <- factor(c("New cases","Hospital cases","Deaths"),
                   levels =c("New cases","Hospital cases","Deaths"))
metric  <- factor(c("new_cases","hospital_cases","deaths") ,
                  levels = c("new_cases","hospital_cases","deaths" ))
niceNames <- data.frame(key = metric, niceName = niceName, stringsAsFactors = T)
plotData <- merge(plotData, niceNames, by = 'key')

gp <- ggplot(plotData, aes(x= dt, y = rolling_avg, colour = lockdown)) + geom_point(aes(colour = lockdown)) + geom_line(aes(colour = lockdown)) + 
  # facet_wrap(~key, scales = 'free') + scale_y_log10() + xlab('Time') + ylab('7 day rolling average')  + 
  facet_grid(rows=vars(niceName), scales = 'free') + scale_y_log10() + 
  xlab('Time') + ylab('7 day rolling average \n with daily figures as thin line')  + 
  geom_line(aes(x=dt, y=value, color=lockdown),  lwd = 0.75) + scale_x_date(date_breaks = "months" , date_labels = "%m-%y") + 
  ggtitle(paste('UK Covid figures on ', max(plotData$dt), sep = '')) + theme_bw(base_size = 18) +
  theme(axis.text.x=element_text(angle=60, hjust=1))
ggsave(gp,filename = 'gov/uk_covid_log.png', height = 6, width = 12)

# add a one week linear regression based on the last 4 weeks of rolling average data
# 
predBase <- plotData %>% filter(key == 'new_cases' & !(is.na(rolling_avg))) %>% select(key, dt, rolling_avg, lockdown, niceName)
predBase <- predBase %>% mutate(rolling_avg_log10 = log10(rolling_avg))
weeksPast <- 4
weeksFuture <- 1.5
predBaseEnd <- max(predBase$dt)
predBase <- predBase %>% filter(dt >= predBaseEnd - (weeksPast * 7))

predStart <- predBaseEnd + 1
predEnd <-   predBaseEnd + round(weeksFuture * 7)
predDates <- seq(predStart, predEnd, by = 'day')

linearModel <- broom::tidy(lm(rolling_avg_log10 ~ dt, predBase))

prediction <- data.frame(key = 'new_cases', dt = predDates, niceName = 'New cases', lockdown = 'Prediction', stringsAsFactors = F)

prediction <- prediction %>% mutate(rolling_avg_log10 = linearModel$estimate[1] + as.numeric(dt) * linearModel$estimate[2],
                     rolling_avg = 10 ** rolling_avg_log10)

gp <- ggplot(plotData, aes(x= dt, y = rolling_avg, colour = lockdown)) + geom_point(aes(colour = lockdown)) + geom_line(aes(colour = lockdown)) + 
  # facet_wrap(~key, scales = 'free') + scale_y_log10() + xlab('Time') + ylab('7 day rolling average')  + 
  facet_grid(rows=vars(niceName), scales = 'free') + scale_y_log10() + 
  xlab('Time') + ylab('7 day rolling average \n with daily figures as thin line')  + 
  geom_line(aes(x=dt, y=value, color=lockdown),  lwd = 0.75) + 
  geom_line(data = prediction, aes(x=dt, y=rolling_avg, color=lockdown),  lwd = 0.75) + 
  scale_x_date(date_breaks = "months" , date_labels = "%m-%y") + 
  ggtitle(paste('UK Covid figures on ', max(plotData$dt), sep = '')) + theme_bw(base_size = 18) +
  theme(axis.text.x=element_text(angle=60, hjust=1))
ggsave(gp,filename = 'gov/uk_covid_log.png', height = 6, width = 12)

  
gp <- ggplot(plotData , aes(x= dt, y = rolling_avg, colour = lockdown)) + geom_point(aes(colour = lockdown)) + geom_line(aes(colour = lockdown)) +   # facet_wrap(~key, scales = 'free') + scale_y_log10() + xlab('Time') + ylab('7 day rolling average')  + 
  facet_grid(rows=vars(niceName), scales = 'free')  + xlab('Time') + ylab('7 day rolling average \n with daily figures as thin line')   + 
  geom_line(aes(x=dt, y=value, color=lockdown),  lwd = 0.75)  + 
  geom_line(data = prediction, aes(x=dt, y=rolling_avg, color=lockdown),  lwd = 0.75) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") + 
  ggtitle(paste('UK Covid figures on ', max(plotData$dt), sep = '')) + theme_bw(base_size = 18)
ggsave(gp,filename = 'gov/uk_covid_lin.png', height = 6, width = 12)



