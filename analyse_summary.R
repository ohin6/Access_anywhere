library(tidyverse)
library(rms)
require(lubridate)
require(ggpubr)
require(tidytext)


##################
# Import dataset #
##################

df = read_csv('rawData/raw_data.csv')

################
# tidy dataset #
################

df = read_csv('rawData/raw_data.csv') %>%
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
  select(patId:accessRating, overallRating, everything()) 

##########################################
# Patient overall satisfaction Age group #
##########################################

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
  ggtitle("Patient Satisfaction\nOverall patient appointments satisfaction across different age groups") +
  xlab('Rating')



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


annotate_figure(ggarrange(f_RLH,f_AUH,f_BGH,
                  nrow = 3,
                  widths = c(1, 1, 0.2), heights = c(3, 3, 1.6),
            common.legend = TRUE,
            legend = "bottom"),
            left = text_grob("Percent", rot = 90))


#################################
# Patient satisfaction per year #
#################################

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
          

#################################
# Most common words in comments #
#################################
#* Create function

Plot2 = function(accessOrOverall, satisfaction, freq) {
  if (accessOrOverall == 'access'){
    df_filter = df %>%
      filter(accessRating %in% satisfaction)
    
    # extract comments
    comment = df_filter$comments
    
    # tidy data
    # remove stop words
    comment = comment %>%
      str_replace_all(pattern = stopwords('english'), replacement = "") #%>%  # remove stop words
    #str_replace_all(string = x, pattern = '[123456789]', replacement = '') # remove numbers
    
    # other changes
    comment = paste(comment, collapse = " ")
    comment = str_replace_all(comment, pattern = '\"', replacement = "") # Remove slashes
    comment = str_replace_all(comment, pattern = '\n', replacement = "") # Remove \n
    comment = str_replace_all(comment, pattern = '\u0092', replacement = "'") #Replace with quote
    comment = str_replace_all(comment, pattern = '\u0091', replacement = "'") #Replace with quote
    
    #Create dataframe
    text_df = data_frame(Text = comment)
    
    text_words = text_df %>% 
      unnest_tokens(output = word, input = Text) 
    
    text_words  = text_words  %>%
      anti_join(stop_words) # Remove stop words (perhaps unnecessary)
    
    text_wordcounts = text_words  %>%
      count(word, sort = TRUE)
    
    ## plot common words ###
    #* Currently set to min freq 100
    text_wordcounts = text_wordcounts %>%
      filter(n > freq) %>% # filter freq accordingly 
      filter(word != 'na') %>%
      mutate(word = reorder(word,n))
    
    plot1 = ggplot(data = text_wordcounts, aes(word, n)) +
      geom_col(fill= 'darkred') +
      coord_flip() +
      geom_text(aes(label = n), hjust = 1.2, colour = 'white', fontface = 'bold') +
      ggtitle(sprintf("Most common words from patient comments who rated\n access satisfaction as '%s'", satisfaction)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(plot1)
  }
  else {
    df_filter = df %>%
      filter(overallRating %in% satisfaction)
    
    # extract comments
    comment = df_filter$comments
    
    # tidy data
    # remove stop words
    comment = comment %>%
      str_replace_all(pattern = stopwords('english'), replacement = "") # remove stop words
    
    # other changes
    comment = paste(comment, collapse = " ")
    comment = str_replace_all(comment, pattern = '\"', replacement = "") # Remove slashes
    comment = str_replace_all(comment, pattern = '\n', replacement = "") # Remove \n
    comment = str_replace_all(comment, pattern = '\u0092', replacement = "'") #Replace with quote
    comment = str_replace_all(comment, pattern = '\u0091', replacement = "'") #Replace with quote
    
    #Create dataframe
    text_df = data_frame(Text = comment)
    
    text_words = text_df %>% 
      unnest_tokens(output = word, input = Text) 
    
    text_words  = text_words  %>%
      anti_join(stop_words) # Remove stop words (perhaps unnecessary)
    
    text_wordcounts = text_words  %>%
      count(word, sort = TRUE)
    
    ## plot common words ###
    #* Currently set to min freq 100
    text_wordcounts = text_wordcounts %>%
      filter(n > freq) %>% # filter freq accordingly 
      filter(word != 'na') %>%
      mutate(word = reorder(word,n))
    
    plot1 = ggplot(data = text_wordcounts, aes(word, n)) +
      geom_col(fill= 'darkred') +
      coord_flip() +
      geom_text(aes(label = n), hjust = 1.2, colour = 'white', fontface = 'bold') +
      ggtitle(sprintf("%s", satisfaction)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab('Frequency') +
      xlab(NULL)
    
    
    return(plot1)
  }
}
##* look through different satisfaction by changing number in [] from 1 (very
##* disasisfied) to 5 (very satisfied) and access/overall satisfaction
f_comSat = Plot2('overall', levels(df$overallRating)[3], 30)
f_comDissat = Plot2('overall', levels(df$overallRating)[1], 14)

ggarrange(f_comSat,f_comDissat)
