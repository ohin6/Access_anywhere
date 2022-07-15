#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rms)
require(lubridate)
require(ggpubr)
require(tidytext)
require(tm)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot <- renderPlot({
      
      df = input
      df = df %>%
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
      
        ggplot(aes(x= overallRating,  group=ageGroup)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                      y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
        labs(y = "Percent", fill="Satisfaction") +
        facet_grid(~ageGroup) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_discrete(labels = levels(df$overallRating)) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        ggtitle("Overall patient appointments satisfaction across different age groups")
      
    })

})
