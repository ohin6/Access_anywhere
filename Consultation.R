library(tidyverse)
require(lubridate)
require(timetk)


########################
# import and tidy data #
########################

cons = read_csv('rawData/consultation.csv', skip = 6) %>%
  rename(Date = `Entry Time (local)`) %>%
  mutate(month = month(Date, label = TRUE)) %>%
  mutate(year = year(Date)) %>%
  mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
                                          'Jun', 'Jul', 'Aug', 
                                          'Sep', 'Oct', 'Nov', 'Dec'))) %>%
  rename(answerTime = `Time To Answer (sec)`) %>%
  rename(abandon = `Is Call Abandoned`) %>%
  rename(rejoin = `# Rejoins`) %>%
  rename(waitingArea = `Waiting Area`)
  

###########################################
##### time to answer per waiting area #####
###########################################

# per site
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == c('RLH','AUH','BGH', 'LUH', 'Other')) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = site, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across different sites') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')

# RLH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'RLH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across The Royal Liverpool Hospital departments') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')


# AUH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'AUH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across Aintree Hospital departments') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')


# Broadgreen
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'BGH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across Broadgreen Hospital departments') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')

# LUH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'LUH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across LUH departments') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')


# Other
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'Other') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,200) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Waiting times across Other departments') +
  xlab('Waiting Area') +
  ylab('Time to answer (mins)')


#############################
##### Number of rejoins #####
#############################

# per site
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == c('RLH','AUH','BGH', 'LUH', 'Other')) %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = site, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across different sites') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')

# RLH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'RLH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across The Royal Liverpool Hospital departments') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')


# AUH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'AUH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across Aintree Hospital departments') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')


# Broadgreen
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'BGH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across Broadgreen Hospital departments') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')

# LUH
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'LUH') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across LUH departments') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')


# Other
cons %>%
  mutate(speciality = waitingArea) %>%
  mutate(site = 'Other') %>%
  mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
  mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
  mutate(site = factor(site, levels = c('RLH','AUH','BGH', 'LUH', 'Other'))) %>%
  filter(site == 'Other') %>%
  mutate(speciality = str_remove(speciality, '^\\w*/')) %>%
  ggplot(aes(x = speciality, y = rejoin, na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  coord_flip() + 
  ggtitle('Number of rejoins across Other departments') +
  xlab('Waiting Area') +
  ylab('Number of rejoins')
