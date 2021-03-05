# common functions
# 


add_lockdown <- function(data.df)
{
  data.df <- data.df %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22') ~ 'Before',
                                                     dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04') ~ '1st Lockdown',
                                                     dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03') ~ 'New normal',
                                                     dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01') ~ 'Lockdown 2.0',
                                                     dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18') ~ 'Newer normal',
                                                     dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19') ~ 'Tier 3',
                                                     dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24') ~ 'Tier 4 part 1',
                                                     dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25') ~ 'X-mas bauble',
                                                     dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04') ~ 'Tier 4 part 2',
                                                     dt >= as.Date('2021-01-05')  ~ 'Lockdown 3.0'))
  
  data.df$lockdown <- factor(data.df$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0'))
  return(data.df)
}
