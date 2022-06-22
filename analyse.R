###################
# Import libaries #
###################
library(tidyverse)
library(rms)
install.packages('AICcmodavg')
require(AICcmodavg)
install.packages('lubridate')
require(lubridate)
install.packages('timetk')
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
  select(patId:accessRating, accessRating.coded, overallRating, overallRating.coded, everything()) %>%
  mutate(accessRating = factor(accessRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neither Satisfied nor Dissatisfied', 'Satisfied', 'Very Satisfied'))) %>%
  mutate(overallRating = factor(overallRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neither Satisfied nor Dissatisfied', 'Satisfied', 'Very Satisfied')))


df$accessRating = factor(df$accessRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neither Satisfied nor Dissatisfied', 'Satisfied', 'Very Satisfied'))

attach(df)

#########################################
# Age groups most satisfied with access #
#########################################

# plot age group satisfaction
ggplot(data = df, aes(x = accessRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Patient satisfaction to accessing video appointments") +
  xlab("Satisfaction") + ylab("count")

# segregate per department 
ggplot(data = df, aes(x = accessRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ department)

##################################################
# Age groups most satisfied with over all rating #
##################################################

# plot age group satisfaction
ggplot(data = df, aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Patient overall experience to video appointments") +
  xlab("Satisfaction") + ylab("count")

# segregate per department 
ggplot(data = df, aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ department)


x = df %>%
  group_by(accessRating) %>%
  summarise_by_time(
    .date_var = start,
    .by       = "month" # Setup for monthly aggregation
  )




