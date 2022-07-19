library(shiny)
library(tidyverse)
library(rms)
require(lubridate)
require(ggpubr)
require(tidytext)
require(tm)
library(rms)
require(stringr)
require(igraph)
require(ggraph)



function(input, output, session) {
  
  ###########################################
  # Create plot for age Groups satisfaction #
  ###########################################
  output$ageGroupPlot = renderPlot({
    #Import data
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df = read_csv(inFile$datapath) %>%
    
    #df = read_csv('../../rawData/raw_data.csv') %>%
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
  
  ################
  # Create table #
  ################
  output$table = renderTable({
    #Import data
    inFile <- input$file1
  
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath) %>%

  
      # df = read_csv('../../rawData/raw_data.csv') %>%
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
      mutate(month = month(start, label = TRUE)) %>%
      mutate(year = year(start)) %>%
      mutate(day = day(start)) %>%
      group_by(year) %>%
      select(patId, day, month, year, overallRating, ageGroup, reUse, Site, department)
  })
  
  ####################
  # Create top words #
  ####################
  output$wordCount = renderPlot({
    #Import data
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    stopwords_filter = stop_words %>%
      filter(lexicon == 'SMART')
    
    df = read_csv(inFile$datapath) %>%
      
      #df = read_csv('../../rawData/raw_data.csv') %>%
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
      group_by(year) %>%
      filter(!is.na(comments)) %>%
      select(patId, (comments)) %>%
      unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
      separate(trigram, c("word1"), sep = " ") %>%
      filter(!word1 %in% stopwords_filter$word) %>%
      filter(!is.na(word1)) %>%
      group_by(patId) %>%
      summarise(words = str_c(word1, collapse = " ")) %>%
      ungroup() %>%
      unnest_tokens(trigram, words, token = "ngrams", n = 2) %>%
      mutate(biwords = trigram) %>%
      separate(trigram, c('word1', 'word2'), ' ')
    
    
    
      
  })
  
}
