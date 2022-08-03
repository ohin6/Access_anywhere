library(shiny)
library(tidyverse)
require(lubridate)
require(ggpubr)
require(tidytext)
require(tm)
require(stringr)
require(igraph)
require(ggraph)
require(textclean)
library(shinydlplot)
library(varhandle)

# Initiate Server

function(input, output, session) {
  
  ########################
  # Import and tidy data #
  ########################
  
  data_input <- reactive({
    #required input file from UI
    req(input$file1)
    # convert UI to CSV
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
      mutate(year = as.integer(year(start))) %>%
      mutate(day = day(start)) %>%
      select(patId, day, month, year, overallRating, ageGroup, reUse, Site, department, comments, start)
  })
  
  #####################
  # Create stop words #
  #####################
  # Function used for filtering comments
  stopwords_filter = reactive({
    stop_words %>%
      filter(lexicon == 'SMART')
  })
  
  ###########################
  # Get dataframe col names #
  ###########################
  
  # Dynamically identify column names from input file (used to choose cols to view in table)      
  #Select columns names
  observeEvent(data_input(), {
    updateSelectInput(session, "select", choices= as.character(colnames(data_input())), selected = 'comments')
  })
  
  #######################################################
  # Dynamically identify unique values from each column #
  #######################################################

                                    
  # ageGroup
  observeEvent(data_input(), {
    updateSelectInput(session, "ageGroup", choices= c('All', sort(as.character(unique(data_input()$ageGroup)))))
  })
  
  #Year
  observeEvent(data_input(), {
    updateSelectInput(session, "Year", choices= c('All', sort(unique(data_input()$year))))
  })
  

  listOfMonths <- function(months){
    months <- factor(months, levels = month.abb)
    months <- sort(months)
    months <- month.abb[months]
    return(months)
    }
  
  
  # Month
  observeEvent(data_input(), {
    updateSelectInput(session, "month", choices= c('All', listOfMonths(unique(data_input()$month))))
  })
  
  #Site
  observeEvent(data_input(), {
    updateSelectInput(session, "site", choices= c('All',unique(data_input()$Site)))
  })
  
  # Department
  observeEvent(data_input(), {
    updateSelectInput(session, "department", choices= c('All', sort(unique(data_input()$department))))
  })
  
  #satistfaction
  observeEvent(data_input(), {
    updateSelectInput(session, "satisfaction", choices= c('All', sort(as.character(unique(data_input()$overallRating)))))
  })
  
  
  ######################
  # Allow regex search #
  ######################
  regex_input <- function(input){
    
    search_string <- str_trim(input, side='both')
    # remove punctuation
    search_string = str_replace_all(search_string, "[^[:alnum:]]", " ")
    # split string into elements
    search_string <- unlist(str_split(search_string,"\\s{1,5}"))
    
    
    # loop through string to add regex
    search_input<-'\\b'
    for (i in 1:length(search_string)){
      search_input <- paste(search_input, search_string[i][1], "")
      if (i <= length(search_string) -1) {
        search_input <- paste(search_input,'\\W+(?:\\w+\\W+){0,4}?',"")
      }
    }
    search_input <- paste(search_input,'\\b',"")
    search_input <- str_replace_all(search_input, fixed(" "), "")
    return(search_input)
  }
  
  
  ################
  # Create table #
  ################
  
  # Create table 
  data <- reactive({
    # required data variable (see above)
    req(data_input()) %>%
      # remove punctuation as this was causing issues in table
      mutate(comments = str_replace_all(comments, "[^[:alnum:]]", " ")) %>% 
      # Add filters (from main panel)
      filter(if(input$comments!= 'No') (!is.na(comments)) else TRUE) %>%
      filter(if(input$searchTerm!= '') (str_detect(comments, regex(regex_input(input$searchTerm), ignore_case = TRUE)))  else TRUE) %>%
      
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
  })
  
  #render data table
  output$table = renderTable({
    if (is.null(data()))
      return(NULL)
    # read in function for tidied table
    data() %>%
      select(input$select)
  })
  
  # count rows in filtered table
  output$tableCount = renderText({
    if (is.null(data()))
      return(NULL)
    # read in function for tidied table
    data() %>%
      nrow()
  })
  
  
  ###########################################
  # Create plot for age Groups satisfaction #
  ###########################################
  
  # create plot variable
  plotAgeSat = reactive({
    # required data variable (see above)
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
    ggarrange(df %>%
                ggplot(aes(x= overallRating,  group=ageGroup)) + 
                geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
                geom_text(aes(label = scales::percent(..prop.., accuracy = 0.1),
                              y= ..prop.. ), stat= "count", vjust = -.5, size = 2) +
                labs(y = "Percent", fill="Satisfaction") +
                facet_grid(~ageGroup) +
                scale_y_continuous(labels = scales::percent) +
                scale_fill_discrete(labels = levels(df$overallRating)) +
                theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
                ggtitle("Overall patient appointments satisfaction across different age groups") +
                xlab('Rating'), legend= 'bottom')
  })
  
  # render plot
  output$ageGroupPlot = renderPlot({
    if (is.null(data_input()))
      return(NULL)
    # call above variable
    plotAgeSat()
  })
  
  
  #############################
  # Create table for ageGroup #
  #############################
  
  # create plot variable
  df2 = reactive({
    req(data_input())
      # create data table variable
      agetable = tibble()

      # # fill table value using loop 
      # for (i in 1:length(unique(data()$ageGroup))){
      #   for (j in 1:length(unique(data()$overallRating))){
      #     count = data() %>%
      #       select(ageGroup,overallRating) %>%
      #       filter(overallRating == unique(unfactor(overallRating))[j]) %>%
      #       filter(ageGroup == unique(unfactor(ageGroup))[i]) %>%
      #       nrow()
      #     agetable[j,i] = count
      #   }
      #   colnames(agetable) = unique(data()$ageGroup)
      # }
      
      agetable = tibble()
      index_i <- 1
      # fill table value using loop 
      for (i in sort(unique(data()$ageGroup))){
        index_j <- 1 
        for (j in sort(unique(data()$overallRating))){
          count = data() %>%
            select(ageGroup,overallRating) %>%
            filter(overallRating == j) %>%
            filter(ageGroup == i) %>%
            nrow()
          agetable[index_j,index_i] = as.integer(count)
          index_j <- index_j +1 
          colnames(agetable) = sort(unique(data()$ageGroup))
        }
        index_i <- index_i +1 
      }
      
      
      # add rating column
      agetable = agetable %>% 
        mutate(Rating = sort(unique(data()$overallRating))) %>%
        select(Rating, everything())%>%
        mutate(Total = as.integer(rowSums(agetable)))
  })
  
  # Render table
  output$ageTable = renderTable({
    if (is.null(data_input()))
      return(NULL)
    # read in function for tidied table
    df2()
  })
  
  
  ############################
  # Create top words 2 words #
  ############################
  
  plotBigram = reactive({
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
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
  
  # plot variable
  output$wordCount = renderPlot({
    # call above variable
    if (is.null(data_input()))
      return(NULL)
    plotBigram()
  })
  
  ############################
  # Create top words 3 words #
  ############################
  
  # Create plot variable
  plotTrigram = reactive({
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
    
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
  
  # Plot above variable 
  output$wordCount2 = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    plotTrigram()
  })
  
  
  
  ##################
  # igraph network #
  ##################
  
  # create plot variable
  plotiGraph = reactive({
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
    # Create bigram object
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
    
    # bigram connections
    bigram_graph <- Bigram %>%
      count(biwords, sort = TRUE) %>%
      separate(biwords, c('word1', 'word2'), ' ') %>%
      filter(n > 7) %>%
      graph_from_data_frame()
    
    #plot igraph
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
  
  # Plot variable  
  output$wordCount3 = renderPlot({
    
    
    if (is.null(data_input()))
      return(NULL)
    # call above plot
    plotiGraph()
  })
  
  #########################
  # Satisfaction Per year #
  #########################
  
  # create plot variable
  plotSatYear = reactive ({
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
    
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
  
  # plot above variable
  output$perYear = renderPlot({
    if (is.null(data_input()))
      return(NULL)
    # call above variable
    plotSatYear()
  })
  
  
  
  ##########################################
  # Create table for satisfaction per year #
  ##########################################
 
  # Create table variable
  # tableYear = reactive({
  #   req(data_input())
  #   df3 = tibble ()
  #   for (i in 1:length(unique(data()$year))){
  #     for (j in 1:length(unique(data()$overallRating))){
  #       count = data() %>% 
  #         select(Site,year,overallRating) %>%
  #         filter(overallRating == unique(unfactor(overallRating))[j]) %>%
  #         filter(year == unique(year)[i]) %>%
  #         nrow()
  #       df3[j,i] = count
  #     }
  #     colnames(df3) = unique(data()$year)
  #   }
  #   df3 %>%
  #     mutate(Rating = unique(data()$overallRating)) %>%
  #     select(Rating, everything()) %>%
  #     mutate(Total = as.integer(rowSums(df3)))
  # })

  tableYear = reactive({
  req(data_input())
  df3 = tibble()
  index_i <- 1
  # fill table value using loop 
  for (i in sort(unique(data()$year))){
    index_j <- 1 
    for (j in sort(unique(data()$overallRating))){
      count = data() %>%
        select(year,year,overallRating) %>%
        filter(overallRating == j) %>%
        filter(year == i) %>%
        nrow()
      df3[index_j,index_i] = as.integer(count)
      index_j <- index_j +1 
      colnames(df3) = sort(unique(data()$year))
    }
    index_i <- index_i +1 
  }
  df3 %>%
        mutate(Rating = sort(unique(data()$overallRating))) %>%
        select(Rating, everything()) %>%
        mutate(Total = as.integer(rowSums(df3)))
    })
  
  
  
  # render table
  output$tableYEAR = renderTable({
    if (is.null(data_input()))
      return(NULL)
    # call above variable
    tableYear()
  })
  
  
  ###############################
  # Satisfaction Per department #
  ###############################
  
  # Create plot variable
  plotSatDep = reactive({
    req(data_input())
    # read in function for tidied table
    df = data_input() %>%
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE)
    
    # Set filter for departments required sample size
    x = as.data.frame(table(df$department)) %>%
      filter(Freq > input$minSample)
    
    # plot Royal
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
      scale_fill_discrete(labels = levels(df$overallRating)) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Royal Liverpool Hospital', ) + 
      xlab(NULL) + 
      ylab(NULL)
    
    # Plot Aintree
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
      scale_fill_discrete(labels = levels(df$overallRating)) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Aintree Hospital') + 
      xlab(NULL) + 
      ylab(NULL)
    
    # Plot Broadgreen
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
      scale_fill_discrete(labels = levels(df$overallRating)) +
      theme(text = element_text(size=8),
            axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
            strip.text = element_text(size = 5), plot.title = element_text(size = 8, face = 'bold')) +
      ggtitle('Broadgreen Hospital') + 
      xlab(NULL) + 
      ylab(NULL)
    
    # Concatenate plots based on user selection (radio buttons)
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
  
  #plot above variable
  output$department = renderPlot({
    
    if (is.null(data_input()))
      return(NULL)
    # call above variable
    plotSatDep()
  })
  
  
  ################################
  # Create table for Site counts #
  ################################
  
  # Create variable
  tableSite = reactive({
    req(data_input())
    # Create empty dataframe 
    df = tibble()
    # fill values
    for (i in 1:length(unique(data()$Site))){
      for (j in 1:length(unique(data()$year))){
        count = data() %>% 
          filter(Site == unique(Site)[i],
                 year == unique(year)[j]) %>%
          nrow()
        df[i,j] = count
      }
    }
    
    # add column names
    colnames(df) = unique(data()$year)
    
    # add column for site
    df2 = df %>%
      mutate(`Total(count)` = as.integer(rowSums(df))) %>%
      mutate(Site = unique(data()$Site))%>%
      select(Site, everything())
    
    # Toggle between percent and count values
    # Create function for converting to percentage 
    scale2 <- function(x) ((x / rowSums(df)) * 100)
    
    # toggle based on input from radio button
    if (input$sitePercent == 'Percent'){
      #mutate across all but first  and last column to give percentage
      df2 %>% mutate(across(2:(ncol(df2)-1), scale2))
    } else
      df2
  })
  
  # Render table
  output$tableSITE = renderTable({
    if(is.null(data_input()))
      return(NULL)
    tableSite()
  })

  #######################
  # Count sample inputs #
  #######################
  output$count = renderText({
    
    if (is.null(data_input()))
      return('No file selected')
    
    # read in function for tidied table
    data_input() %>%
      # add filters
      # Add filters (from side panel)
      filter(if(input$ageGroup!= 'All') (ageGroup == input$ageGroup) else TRUE) %>%
      filter(if(input$Year != 'All')  (year == input$Year) else TRUE) %>%
      filter(if(input$month!= 'All') (month == input$month) else TRUE) %>%
      filter(if(input$site!= 'All') (Site == input$site) else TRUE) %>%
      filter(if(input$department!= 'All') (department == input$department) else TRUE) %>%
      filter(if(input$satisfaction!= 'All') (overallRating == input$satisfaction) else TRUE) %>%
      # Count rows
      nrow()
  })
  
  
  ####################
  # Download buttons #
  ####################
  
  # Data Table
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataTable-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data(), file)
    })
  
  # Age Group Satisfaction
  output$downloadageGroup = downloadHandler(
    filename = function() {
      paste("AgeGroup-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotAgeSat(), device = device)
    })
  
  # Bigram
  output$downloadBigram = downloadHandler(
    filename = function() {
      paste("Bigram-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotBigram(), device = device)
    })
  
  # Trigram
  output$downloadTrigram = downloadHandler(
    filename = function() {
      paste("Trigram-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotTrigram(), device = device)
    })
  
  # iGraph
  output$downloadiGraph = downloadHandler(
    filename = function() {
      paste("igraph-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotiGraph(), device = device)
    })
  
  # Satisfaction per Year
  output$downloadYear = downloadHandler(
    filename = function() {
      paste("SatYear-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotSatYear(), device = device)
    })
  
  # Satisfaction Per Department
  output$downloadSatDep = downloadHandler(
    filename = function() {
      paste("SatDep-", Sys.Date(), ".png", sep="")
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotSatDep(), device = device)
    })
}