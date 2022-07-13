library(tidyverse)
require(lubridate)
require(timetk)
require(rms)


########################
# import and tidy data #
########################

cons = read_csv('rawData/consultation.csv', skip = 6) %>%
  mutate(Date = dmy_hm(`Entry Time (local)`)) %>%
  mutate(month = month(Date, label = TRUE)) %>%
  mutate(year = year(Date)) %>%
  mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
                                          'Jun', 'Jul', 'Aug', 
                                          'Sep', 'Oct', 'Nov', 'Dec'))) %>%
  rename(answerTime = `Time To Answer (sec)`) %>%
  rename(abandon = `Is Call Abandoned`) %>%
  rename(rejoin = `# Rejoins`) %>%
  rename(waitingArea = `Waiting Area`)

############################
# Uptake different regions #
############################

# All Sites all departments

cons %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
  mutate(site = factor(site, levels = c('AUH', 'BGH', 'LUH', 'RLH', 'Other'))) %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) +
  geom_freqpoly(binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle("Uptake of Attend Anywhere during 2020-2022") +
  ylab('Count (per week)') +
  xlab("Time (Month)")

# Different Sites facet_grid

cons %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) +
  geom_freqpoly(binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  facet_grid(site ~ .) +
  ggtitle("Uptake of Attend Anywhere during 2020-2022 per site") +
  ylab('Count (per week)') +
  xlab("Time (Month)")

#Facet_wrap

cons %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) +
  geom_freqpoly(binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  facet_wrap(~ site) +
  ggtitle("Uptake of Attend Anywhere during 2020-2022 per site") +
  ylab('Count (per week)') +
  xlab("Time (Month)")

# Grouped

group.colours = c('RLH' = 'red',
                  'AUH' = 'orange',
                  'BGH' = 'green',
                  'LUH' = 'blue',
                  'Other' = 'grey')

cons %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date, colour= site)) +
  geom_freqpoly(binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  scale_colour_manual(values=group.colours) +
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle("Uptake of Attend Anywhere during 2020-2022 per site") +
  ylab('Count (per week)') +
  xlab("Time (Month)")


#######################################
# Department uptake per site per site #
#######################################
  
#### Royal Liverpool #####


cons %>% 
  mutate(speciality = waitingArea) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'RLH') %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) + 
  geom_freqpoly(binwidth = 7) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  facet_wrap(~ speciality) + 
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle('Weekly uptake of Attend Anywhere across different departments at The Royal Liverpool Hospital') +
  ylab('Count (per week)') +
  xlab('Date')

#### Aintree #####
  
cons %>% 
  mutate(speciality = waitingArea) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'AUH') %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) + 
  geom_freqpoly(binwidth = 7) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  facet_wrap(~ speciality) + 
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle('Weekly uptake of Attend Anywhere across different departments at the Aintree Hospital') +
  ylab('Count (per week)') +
  xlab('Date')

#### BroadGreen #####
  
cons %>% 
  mutate(speciality = waitingArea) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'BGH') %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) + 
  geom_freqpoly(binwidth = 7) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  facet_wrap(~ speciality) + 
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle('Weekly uptake of Attend Anywhere across different departments at The BroadGreen Hospital') +
  ylab('Count (per week)') +
  xlab('Date')

#### Liverpool Hospital #####
  
cons %>% 
  mutate(speciality = waitingArea) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'LUH') %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) + 
  geom_freqpoly(binwidth = 7) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  facet_wrap(~ speciality) + 
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle('Weekly uptake of Attend Anywhere across different departments at Liverpool Hospital') +
  ylab('Count (per week)') +
  xlab('Date')

#### Miscellaneous #####

cons %>% 
  mutate(speciality = waitingArea) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'Other') %>%
  mutate(Date = as_date(Date)) %>%
  ggplot(aes(Date)) + 
  geom_freqpoly(binwidth = 7) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") + 
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
  facet_wrap(~ speciality) + 
  theme(axis.text.x = element_text(size=8, angle=45)) + 
  ggtitle('Weekly uptake of Attend Anywhere across different departments Miscellaneous') +
  ylab('Count (per week)') +
  xlab('Date')
