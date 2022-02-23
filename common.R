# common functions
# 


add_lockdown <- function(data.df)
{
  data.df <- data.df %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22')                                ~ 'Before',
                                                     dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04')  ~ '1st Lockdown',
                                                     dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03')  ~ 'New normal',
                                                     dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01')  ~ 'Lockdown 2.0',
                                                     dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18')  ~ 'Newer normal',
                                                     dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19')  ~ 'Tier 3',
                                                     dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24')  ~ 'Tier 4 part 1',
                                                     dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25')  ~ 'X-mas bauble',
                                                     dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04')  ~ 'Tier 4 part 2',
                                                     dt >= as.Date('2021-01-05') & dt <= as.Date('2021-03-07')  ~ 'Lockdown 3.0',
                                                     dt >= as.Date('2021-03-08') & dt <= as.Date('2021-03-28')  ~ 'Step 1 pt.1',
                                                     dt >= as.Date('2021-03-29') & dt <= as.Date('2021-04-11')  ~ 'Step 1 pt.2',
                                                     dt >= as.Date('2021-04-12') & dt <= as.Date('2021-05-16')  ~ 'Step 2',
                                                     dt >= as.Date('2021-05-17') & dt <= as.Date('2021-06-20')  ~ 'Step 3',
                                                     dt >= as.Date('2021-06-21') & dt <= as.Date('2021-07-18')  ~ 'Step 3.5',
                                                     dt >= as.Date('2021-07-19') & dt <= as.Date('2021-12-09')  ~ 'End of the Pandemic', 
                                                     dt >= as.Date('2021-12-10') & dt <= as.Date('2022-01-19')  ~ 'Plan B',
                                                     dt >= as.Date('2022-01-20')                                ~ 'End of pandemic again'))
  
                               
  
  data.df$lockdown <- factor(data.df$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0','Step 1 pt.1','Step 1 pt.2','Step 2','Step 3','Step 3.5','End of the Pandemic','Plan B','End of pandemic again'))
  return(data.df)
}




add_lockdown_pred <- function(data.df)
{
  data.df <- data.df %>% mutate(lockdown = case_when(dt <= as.Date('2020-03-22')                                ~ 'Before',
                                                     dt >= as.Date('2020-03-23') & dt <= as.Date('2020-07-04')  ~ '1st Lockdown',
                                                     dt >= as.Date('2020-07-05') & dt <= as.Date('2020-11-03')  ~ 'New normal',
                                                     dt >= as.Date('2020-11-04') & dt <= as.Date('2020-12-01')  ~ 'Lockdown 2.0',
                                                     dt >= as.Date('2020-12-02') & dt <= as.Date('2020-12-18')  ~ 'Newer normal',
                                                     dt >= as.Date('2020-12-19') & dt <= as.Date('2020-12-19')  ~ 'Tier 3',
                                                     dt >= as.Date('2020-12-20') & dt <= as.Date('2020-12-24')  ~ 'Tier 4 part 1',
                                                     dt >= as.Date('2020-12-25') & dt <= as.Date('2020-12-25')  ~ 'X-mas bauble',
                                                     dt >= as.Date('2020-12-26') & dt <= as.Date('2021-01-04')  ~ 'Tier 4 part 2',
                                                     dt >= as.Date('2021-01-05') & dt <= as.Date('2021-03-07')  ~ 'Lockdown 3.0',
                                                     dt >= as.Date('2021-03-08') & dt <= as.Date('2021-03-28')  ~ 'Step 1 pt.1',
                                                     dt >= as.Date('2021-03-29') & dt <= as.Date('2021-04-11')  ~ 'Step 1 pt.2',
                                                     dt >= as.Date('2021-04-12') & dt <= as.Date('2021-05-16')  ~ 'Step 2',
                                                     dt >= as.Date('2021-05-17') & dt <= as.Date('2021-06-20')  ~ 'Step 3',
                                                     dt >= as.Date('2021-06-21') & dt <= as.Date('2021-07-18')  ~ 'Step 3.5',
                                                     dt >= as.Date('2021-07-19') & dt <= as.Date('2021-12-09')  ~ 'End of the Pandemic', 
                                                     dt >= as.Date('2021-12-10') & dt <= as.Date('2022-01-19')  ~ 'Plan B',
                                                     dt >= as.Date('2022-01-20')                                ~ 'End of pandemic again'))
  
  
  
  data.df$lockdown <- factor(data.df$lockdown, levels = c("Before","1st Lockdown","New normal","Lockdown 2.0", 'Newer normal','Tier 3','Tier 4 part 1','X-mas bauble','Tier 4 part 2','Lockdown 3.0','Step 1 pt.1','Step 1 pt.2','Step 2','Step 3','Step 3.5','End of the Pandemic','Plan B','End of pandemic again','prediction'))
  return(data.df)
}
