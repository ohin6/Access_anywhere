###################
# Import libaries #
###################
library(tidyverse)
library(rms)

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
  select(patId:accessRating, accessRating.coded, overallRating, overallRating.coded, everything())

attach(df)

ggplot(data = df) + 
  geom_bar(aes(accessRating)) + 
  facet_wrap(~ageGroup)


ggplot(data = df, aes(accessRating)) + 
  geom_bar(aes(fill = ageGroup))

df %>% filter(ageGroup)

table(accessRating)


plot(ageGroup,accessRating)


describe(df)

table(df$`Would you be happy to use the Video Consultation again?`)

table(df$`Please rate the ease of use for accessing the Video Consultation today?`)
