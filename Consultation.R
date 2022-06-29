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
  


# time to answer per waiting area

cons %>%
ggplot(aes(x = waitingArea, y = answerTime/60, na.rm = TRUE)) +
  ylim(0,100) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  ylab('Time to answer (mins)') +
  coord_flip()

# Number of rejoins per waiting area#
cons %>%
  ggplot(aes(x = waitingArea, y = rejoin , na.rm = TRUE)) +
  ylim(0,20) +
  geom_boxplot() + 
  theme(axis.text=element_text(size=8)) +
  ylab('Number of times rejoined') +
  coord_flip()
