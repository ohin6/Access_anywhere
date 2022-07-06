
###################
# Import libaries #
###################
install.packages('lubridate')
install.packages('timetk')
install.packages('AICcmodavg')
library(tidyverse)
library(rms)
require(AICcmodavg)
require(lubridate)
require(timetk)

##################
# Import dataset #
##################

df = read_csv('rawData/raw_data.csv')

################
# tidy dataset #
################

df = df%>%
  rename(patId = "Respondent ID") %>%
  rename(colId = "Collector ID") %>%
  rename(start = "Start Date") %>%
  rename(end = "End Date") %>%
  rename(ageGroup = "What age group do you belong?") %>%
  rename(accessRating = "Please rate the ease of use for accessing the Video Consultation today?") %>%
  rename(overallRating = "Please rate your overall experience of the Video Consultation today?") %>%
  rename(reUse = "Would you be happy to use the Video Consultation again?") %>%
  rename(comments = "Please provide us with any additional comments/ suggestions.") %>%
  rename(department = "Waiting Area") %>%
  mutate(accessRating.coded = as.numeric(NA))%>%
  mutate(accessRating.coded = if_else(accessRating == 'Very Dissatisfied', 0, accessRating.coded)) %>%
  mutate(accessRating.coded = if_else(accessRating == 'Dissatisfied', 1, accessRating.coded )) %>%
  mutate(accessRating.coded = if_else(accessRating == 'Neither Satisfied nor Dissatisfied', 2, accessRating.coded )) %>%
  mutate(accessRating.coded = if_else(accessRating == 'Satisfied', 3, accessRating.coded )) %>%
  mutate(accessRating.coded = if_else(accessRating == 'Very Satisfied', 4, accessRating.coded)) %>%
  mutate(overallRating.coded = as.numeric(NA)) %>%
  mutate(overallRating.coded = if_else(overallRating == 'Very Dissatisfied', 0, overallRating.coded )) %>%
  mutate(overallRating.coded = if_else(overallRating == 'Dissatisfied', 1, overallRating.coded )) %>%
  mutate(overallRating.coded = if_else(overallRating == 'Neither Satisfied nor Dissatisfied', 2, overallRating.coded )) %>%
  mutate(overallRating.coded = if_else(overallRating == 'Satisfied', 3, overallRating.coded )) %>%
  mutate(overallRating.coded = if_else(overallRating == 'Very Satisfied', 4, overallRating.coded)) %>%
  mutate(Site = 'Other') %>%
  mutate(Site = if_else(str_detect(department, 'RLH') == TRUE, 'RLH', Site)) %>%
  mutate(Site = if_else(str_detect(department,'AUH') == TRUE, 'AUH', Site)) %>%
  mutate(Site = if_else(str_detect(department,'LUH') == TRUE, 'LUH', Site)) %>%
  mutate(Site = if_else(str_detect(department,'BGH') == TRUE, 'BGH', Site)) %>%
  # mutate(Site = factor(Site, levels = c('RLH', 'BGH', AUH', 'LUH', 'Other'))) %>%
  select(patId:accessRating, accessRating.coded, overallRating, overallRating.coded, everything()) 
 
#replace strings
df$accessRating = str_replace(df$accessRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')
df$overallRating = str_replace(df$overallRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')

# reordering satisfaction levels and age groups

df = df%>%
  mutate(accessRating = factor(accessRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))) %>%
  mutate(overallRating = factor(overallRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))) %>%
  mutate(ageGroup = factor(ageGroup, c('16 - 25', '26 - 35', '36 - 45', '46 - 65', '65+')))

nrow(df)
table(df$Site)
sum(table(df$Site))
#########################################
# Age groups most satisfied with access #
#########################################


# satisfaction by age group percent
df %>%
  ggplot(aes(x= accessRating,  group=ageGroup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill="Satisfaction") +
  facet_grid(~ageGroup) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = levels(df$accessRating)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Patient satisfaction\nAccessing patient appointments across different age groups")


# segregate per department - filtered with departments with > 100 samples
x = as.data.frame(table(df$department)) %>%
  filter(Freq > 100)

df %>%
  filter(department %in% x$Var1) %>%
  ggplot(aes(x= accessRating,  group=department)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~department) +
  scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(ylim = c(0, 0.85)) +
  scale_fill_discrete(name = "Access Rating", 
                      labels = c("Very Dissatisified", "Dissatisfied", "Neutral",
                                 "Satisified", "Very Satisfied")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        strip.text = element_text(size = 5))


# segregate by month
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>% 
  mutate(month = factor(month, levels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 
                                          'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                          'Feb', 'Mar'))) %>%
  group_by(month) %>%
  ggplot(aes(x = accessRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ month) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Patient satisfaction to accessing video appointments per month") +
  xlab("Satisfaction") + ylab("count")


# segregate by month - 2020
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2020) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                       'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                       'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = accessRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments per month in 2020") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

# segregate by month - 2021
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2021) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = accessRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments per month in 2021") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

# segregate by month - 2022
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2022) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = accessRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments per month in 2022") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))


# segregate by year
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(year) %>%
  ggplot(aes(x = accessRating, group = year)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ year) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments per year") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))


# Segregate by site

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2020) %>%
  ggplot(aes(x = accessRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments for each site in 2020") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2022) %>%
  filter(Site == 'RLH') %>%
  nrow()




df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2021) %>%
  ggplot(aes(x = accessRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments for each site in 2021") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2022) %>%
  ggplot(aes(x = accessRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'accessRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$accessRating)) +
  ggtitle("Patient satisfaction to accessing video appointments for each site in 2022") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))


##################################################
# Age groups most satisfied with over all rating #
##################################################

# plot age group satisfaction
ggplot(data = df, aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Global Patient satisfaction to accessing video appointments") +
  xlab("Satisfaction") + ylab("count")

# satisfaction by age group percent
df %>%
  ggplot(aes(x= overallRating,  group=ageGroup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill="Satisfaction") +
  facet_grid(~ageGroup) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = levels(df$overallRating)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Patient Satisfaction\nOverall patient appointments satisfaction across different age groups")


# segregate per department - filtered with departments with > 100 samples

x = as.data.frame(table(df$department)) %>%
  filter(Freq > 100)

df %>%
  filter(department %in% x$Var1) %>%
  ggplot(aes(x= overallRating,  group=department)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~department) +
  scale_y_continuous(labels = scales::percent) + 
  coord_cartesian(ylim = c(0, 0.85)) +
  scale_fill_discrete(name = "Overall Rating", 
                      labels = c("Very Dissatisified", "Dissatisfied", "Neutral",
                                 "Satisified", "Very Satisfied")) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        strip.text = element_text(size = 5))


# segregate by month
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(month = factor(month, levels = c('Apr', 'May', 'Jun', 'Jul', 'Aug', 
                                          'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 
                                          'Feb', 'Mar'))) %>%
  group_by(month) %>%
  ggplot(aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ month) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle("Patient satisfaction to accessing video appointments per month") +
  xlab("Satisfaction") + ylab("count")


# segregate by month - 2020
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2020) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                       'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                       'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = overallRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments per month in 2020") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

# segregate by month - 2021
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2021) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = overallRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments per month in 2021") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

# segregate by month - 2022
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2022) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(month) %>%
  ggplot(aes(x = overallRating, group = month)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ month) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments per month in 2022") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))


# segregate by year
df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(year) %>%
  ggplot(aes(x = overallRating, group = year)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ year) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments per year") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

# Segregate by site

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2020) %>%
  ggplot(aes(x = overallRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments for each site in 2020") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  filter(year == 2022) %>%
  filter(Site == 'RLH') %>%
  nrow()




df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2021) %>%
  ggplot(aes(x = overallRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments for each site in 2021") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))

df %>%
  mutate(month = month(start, label = TRUE)) %>%
  mutate(year = year(start)) %>%
  # mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
  #                                         'Jun', 'Jul', 'Aug', 'sep', 'Oct',
  #                                         'Nov', 'Dec', ))) %>%
  group_by(Site) %>%
  filter(year == 2022) %>%
  ggplot(aes(x = overallRating, group = Site)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                 y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
  labs(y = "Percent", fill = 'overallRating') +
  facet_wrap(~ Site) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
  scale_fill_discrete(labels = levels(df$overallRating)) +
  ggtitle("Patient overall satisfaction to video appointments for each site in 2022") +
  xlab("Satisfaction") + ylab("Percentage") + 
  coord_cartesian(ylim = c(0, 0.75))
