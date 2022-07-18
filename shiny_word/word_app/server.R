library(shiny)
library(tidyverse)
library(rms)
require(lubridate)
require(ggpubr)
require(tidytext)
require(tm)

function(input, output, session) {
  
  #Import and tidy data
  output$plot = renderPlot({
    df = df = read_csv('../../rawData/raw_data.csv') %>%
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
      mutate(Site = 'Other') %>%
      mutate(Site = if_else(str_detect(department, 'RLH') == TRUE, 'RLH', Site)) %>%
      mutate(Site = if_else(str_detect(department,'AUH') == TRUE, 'AUH', Site)) %>%
      mutate(Site = if_else(str_detect(department,'LUH') == TRUE, 'LUH', Site)) %>%
      mutate(Site = if_else(str_detect(department,'BGH') == TRUE, 'BGH', Site)) %>%
      mutate(accessRating = str_replace(accessRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')) %>%
      mutate(overallRating = str_replace(overallRating, 'Neither Satisfied nor Dissatisfied', 'Neutral')) %>% 
      mutate(overallRating = if_else(overallRating == 'Very Satisfied', 'Satisfied', overallRating)) %>%
      mutate(overallRating = if_else(overallRating == 'Very Dissatisfied', 'Dissatisfied', overallRating)) %>%
      mutate(overallRating = factor(overallRating, levels = c('Dissatisfied', 'Neutral', 'Satisfied'))) %>%
      mutate(ageGroup = factor(ageGroup, c('16 - 25', '26 - 35', '36 - 45', '46 - 65', '65+'))) %>%
      mutate(department = str_remove(department, '^\\w*-')) %>%
      select(patId:accessRating, overallRating, everything()) %>%
      mutate(month = month(start, label = TRUE)) %>%
      mutate(year = year(start)) %>%
      group_by(year)
    
    #plot
    ggplot(df, aes(x = overallRating, group = year)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
      geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                     y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
      labs(y = "Percent", fill = 'Rating') +
      facet_wrap(~ year) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
      scale_fill_discrete(labels = levels(df$overallRating)) +
      ggtitle("Overall patient satisfaction Attend Anywhere per year") +
      xlab("Satisfaction") + ylab("Percentage") + 
      ggtitle('All Sites') + 
      xlab(NULL) + 
      ylab(NULL)
  })
}
