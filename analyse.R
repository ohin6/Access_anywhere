###########
# Problem #
###########

#* Figures looking at satisfaction by age group is biased the sample size per 
#* age group is different. For example we can see that there is a much larger
#* sample size in the older age bracket - table(df$ageGroup)
#* We therefore need to normailise this!





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
  select(patId:accessRating, accessRating.coded, overallRating, overallRating.coded, everything()) 
  
#replace strings
df$accessRating = str_replace(df$accessRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')
df$overallRating = str_replace(df$overallRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')

# reordering satisfaction levels and age groups

df = df%>%
  mutate(accessRating = factor(accessRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))) %>%
  mutate(overallRating = factor(overallRating, levels = c('Very Dissatisfied', 'Dissatisfied', 'Neutral', 'Satisfied', 'Very Satisfied'))) %>%
  mutate(ageGroup = factor(ageGroup, c('16 - 25', '26 - 35', '36 - 45', '46 - 65', '65+')))


#########################################
# Age groups most satisfied with access #
#########################################


# plot age group satisfaction
ggplot(data = df, aes(x = accessRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Global Patient satisfaction to accessing video appointments") +
  xlab("Satisfaction") + ylab("count")

# satisfaction by age group percent
df %>%
  ggplot(aes(x= accessRating,  group=ageGroup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Satisfaction") +
  facet_grid(~ageGroup) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = levels(df$accessRating)) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Patient satisfaction to accessing patient appointments across different age groups")

# satisfaction by age group percent
df %>%
  ggplot(aes(x= ageGroup,  group=accessRating)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  facet_grid(~accessRating) + 
  scale_y_continuous(labels = scales::percent)




# segregate per department 
ggplot(data = df, aes(x = accessRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ department)


# segregate by month
df %>%
  mutate(month = month(start, label = TRUE)) %>%
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


##################################################
# Age groups most satisfied with over all rating #
##################################################


# plot age group satisfaction
ggplot(data = df, aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  ggtitle("Global patient satisfaction to video appointments") +
  xlab("Satisfaction") + ylab("count")

# satisfaction by age group percent
df %>%
  ggplot(aes(x= overallRating,  group=ageGroup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Satisfaction") +
  facet_grid(~ageGroup) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = levels(df$overallRating)) +
  theme(axis.text.x = element_blank()) +
  ggtitle("Patient overall satisfaction to video appointments across different age groups")

# satisfaction by age group percent
df %>%
  ggplot(aes(x= ageGroup,  group=overallRating)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  facet_grid(~overallRating) + 
  scale_y_continuous(labels = scales::percent)


# segregate per department 
ggplot(data = df, aes(x = overallRating, fill = ageGroup)) + 
  geom_bar(position = 'dodge') + 
  facet_wrap(~ department)


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

