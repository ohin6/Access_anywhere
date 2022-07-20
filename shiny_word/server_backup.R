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
require(textclean)



function(input, output, session) {
  
  ########################
  # Import and tidy data #
  ########################
  
  data_input <- reactive({
    req(input$file1)
    read_csv(input$file1$datapath) %>%
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
      mutate(day = day(start))
    
  })
  
  #####################
  # Create stop words #
  #####################
  stopwords_filter = reactive({
    stop_words %>%
      filter(lexicon == 'SMART')
  })
  
  ###########################################
  # Create plot for age Groups satisfaction #
  ###########################################
  output$ageGroupPlot = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input() %>%
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
    
    if (is.null(data_input()))
      return(NULL)
    # read in function for tidied table
    data_input() %>%
      # select data for table
      select(patId, day, month, year, overallRating, ageGroup, reUse, Site, department)
  })
  
  ############################
  # Create top words 2 words #
  ############################
  output$wordCount = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input()
    
    Bigram = df %>%
      filter(!is.na(comments)) %>%
      select(patId, (comments)) %>%
      unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
      separate(trigram, c("word1"), sep = " ") %>%
      filter(!word1 %in% stopwords_filter()$word) %>% # filtered using stop words function
      filter(!is.na(word1)) %>%
      group_by(patId) %>%
      summarise(words = str_c(word1, collapse = " ")) %>%
      ungroup() %>%
      unnest_tokens(trigram, words, token = "ngrams", n = 2) %>%
      mutate(biwords = trigram) %>%
      separate(trigram, c('word1', 'word2'), ' ')
    
    # plot top 20 biwords
    
    Bigram %>%
      count(biwords, sort = TRUE) %>% 
      filter(!is.na(biwords)) %>%
      slice(1:input$integer) %>%
      ggplot(aes(x = reorder(biwords, n), y = n)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      xlab(NULL) +
      ylab('Count')
    
  })
  
  ############################
  # Create top words 3 words #
  ############################
  output$wordCount2 = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input()
    
    
    # collate triwords
    
    triwords = df %>%
      filter(!is.na(comments)) %>%
      select(patId, (comments)) %>%
      unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
      separate(trigram, c("word1"), sep = " ") %>%
      filter(!word1 %in% stopwords_filter()$word) %>%
      filter(!is.na(word1)) %>%
      group_by(patId) %>%
      summarise(words = str_c(word1, collapse = " ")) %>%
      ungroup() %>%
      unnest_tokens(trigram, words, token = "ngrams", n = 3) %>%
      mutate(triword = trigram) %>%
      separate(trigram, c('word1', 'word2', 'word3'), ' ')
    
    
    # plot 
    triwords %>%
      count(triword, sort = TRUE) %>% 
      filter(!is.na(triword)) %>%
      slice(1:input$integer) %>%
      ggplot(aes(x = reorder(triword, n), y= n)) +
      geom_bar(stat = 'identity') +
      coord_flip() +
      xlab(NULL) +
      ylab('Count')
    
    
  })
  
  
  ##################
  # igraph network #
  ##################
  output$wordCount3 = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input()
    
    Bigram = df %>%
      filter(!is.na(comments)) %>%
      select(patId, (comments)) %>%
      unnest_tokens(trigram, comments, token = "ngrams", n = 1) %>%
      separate(trigram, c("word1"), sep = " ") %>%
      filter(!word1 %in% stopwords_filter()$word) %>%
      filter(!is.na(word1)) %>%
      group_by(patId) %>%
      summarise(words = str_c(word1, collapse = " ")) %>%
      ungroup() %>%
      unnest_tokens(trigram, words, token = "ngrams", n = 2) %>%
      mutate(biwords = trigram) %>%
      separate(trigram, c('word1', 'word2'), ' ')
    
    
    bigram_graph <- Bigram %>%
      count(biwords, sort = TRUE) %>%
      separate(biwords, c('word1', 'word2'), ' ') %>%
      filter(n > 7) %>%
      graph_from_data_frame()
    
    
    a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
    set.seed(1)
    ggraph(bigram_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                     arrow = a, end_cap = circle(.07, 'inches'), check_overlap = FALSE) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 3) +
      theme_void() +
      ggtitle("bigram network, n > 7")
    
    
  })
  #########################
  # Satisfaction Per year #
  #########################
  output$perYear = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input()
    
    f_all = df %>%
      mutate(month = month(start, label = TRUE)) %>%
      mutate(year = year(start)) %>%
      group_by(year) %>%
      ggplot(aes(x = overallRating, group = year)) + 
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
    
    
    f_site = df %>%
      filter(Site %in% c('AUH', 'RLH', 'BGH')) %>%
      mutate(Site = factor(Site, levels = c('AUH', 'RLH', 'BGH'))) %>%
      mutate(month = month(start, label = TRUE)) %>%
      mutate(year = year(start)) %>%
      group_by(year) %>%
      ggplot(aes(x = overallRating, group = year)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
      geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                     y= ..prop.. ), stat= "count", size = 2) +
      labs(y = "Percent", fill = 'Rating') +
      facet_grid(Site ~ year) +
      scale_y_continuous(labels = scales::percent) +
      theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) + 
      scale_fill_discrete(labels = levels(df$overallRating)) +
      ggtitle('Per Site') + 
      xlab(NULL) + 
      ylab(NULL)
    
    annotate_figure(ggarrange(f_all, f_site,
                              common.legend = TRUE, legend = 'bottom'),
                    left = text_grob("Percent", rot = 90))
    
  })
  
  
  ###############################
  # Satisfaction Per department #
  ###############################
  output$department = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    
    # read in function for tidied table
    df = data_input()
    
    x = as.data.frame(table(df$department)) %>%
      filter(Freq > 100)
    
    f_RLH = df %>%
      filter(Site == 'RLH') %>%
      filter(department %in% x$Var1) %>%
      ggplot(aes(x= overallRating,  group=department)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                     y= ..prop.. ), stat= "count", vjust = 0, size = 2) +
      labs(y = "Percent", fill = 'overallRating') +
      facet_wrap(~ department, ncol = 6) +
      scale_y_continuous(labels = scales::percent) + 
      # coord_cartesian(ylim = c(0, 0.)) +
      scale_fill_discrete(name = "Overall Rating", 
                          labels = c("Very Dissatisified", "Dissatisfied", "Neutral",
                                     "Satisified", "Very Satisfied")) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Royal Liverpool Hospital', ) + 
      xlab(NULL) + 
      ylab(NULL)
    
    f_AUH = df %>%
      filter(Site == 'AUH') %>%
      filter(department %in% x$Var1) %>%
      ggplot(aes(x= overallRating,  group=department)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                     y= ..prop.. ), stat= "count", vjust = 0, size = 2) +
      labs(y = "Percent", fill = 'overallRating') +
      facet_wrap(~ department, ncol = 6) +
      scale_y_continuous(labels = scales::percent) + 
      # coord_cartesian(ylim = c(0, 0.)) +
      scale_fill_discrete(name = "Overall Rating", 
                          labels = c("Very Dissatisified", "Dissatisfied", "Neutral",
                                     "Satisified", "Very Satisfied")) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Aintree Hospital') + 
      xlab(NULL) + 
      ylab(NULL)
    
    f_BGH = df %>%
      filter(Site == 'BGH') %>%
      filter(department %in% x$Var1) %>%
      ggplot(aes(x= overallRating,  group=department)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::percent(..prop.., accuracy = 0.1),
                     y= ..prop.. ), stat= "count", vjust = 0, size = 2) +
      labs(y = "Percent", fill = 'overallRating') +
      facet_wrap(~ department, ncol = 6) +
      scale_y_continuous(labels = scales::percent) + 
      # coord_cartesian(ylim = c(0, 0.)) +
      scale_fill_discrete(name = "Overall Rating", 
                          labels = c("Very Dissatisified", "Dissatisfied", "Neutral",
                                     "Satisified", "Very Satisfied")) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Broadgreen Hospital') + 
      xlab(NULL) + 
      ylab(NULL)
    
    if (input$radio == 'f_RLH'){
      annotate_figure(ggarrange(f_RLH,
                                nrow = 1,
                                widths = c(1, 1, 0.2), heights = c(3, 3, 1.6),
                                common.legend = TRUE,
                                legend = "bottom"),
                      left = text_grob("Percent", rot = 90))
    } else if (input$radio == 'f_AUH'){
      annotate_figure(ggarrange(f_AUH,
                                nrow = 1,
                                widths = c(1, 1, 0.2), heights = c(3, 3, 1.6),
                                common.legend = TRUE,
                                legend = "bottom"),
                      left = text_grob("Percent", rot = 90))
    } else if (input$radio == 'f_BGH'){
      annotate_figure(ggarrange(f_BGH,
                                nrow = 1,
                                widths = c(1, 1, 0.2), heights = c(3, 3, 1.6),
                                common.legend = TRUE,
                                legend = "bottom"),
                      left = text_grob("Percent", rot = 90))
    } else {
      annotate_figure(ggarrange(f_RLH, f_AUH, f_BGH,
                                nrow = 3,
                                widths = c(1, 1, 0.2), heights = c(3, 3, 1.6),
                                common.legend = TRUE,
                                legend = "bottom"),
                      left = text_grob("Percent", rot = 90))
    }
    
    
  })
  
  
}