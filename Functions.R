###################
# Import libraries #
###################

require(tidytext)
require(tm)
require(wordcloud)
library(tidytext) # For unnest_tokens
library(stringr) # For managing text



##########################################
# Create function for wordcloud - Access #
##########################################
accessCloud = function(age, access_satisfaction) {
  df %>%
    filter(ageGroup == age) %>%
    filter(accessRating == access_satisfaction)
  
  # extract comments
  comment = df$comments %>%
    str_replace_all(pattern = stopwords('english'), replacement = "")
  
  # tidy words
  commment_text = Corpus(VectorSource(comment))
  commment_text_clean = tm_map(commment_text, removePunctuation)
  commment_text_clean = tm_map(commment_text_clean, content_transformer(tolower))
  commment_text_clean = tm_map(commment_text_clean, removeNumbers)
  commment_text_clean = tm_map(commment_text_clean, stripWhitespace)
  commment_text_clean = tm_map(commment_text_clean, removeWords, stopwords('english'))
  
  w1 = wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 50, colors = rainbow(30))
  w2 = wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 70, colors = rainbow(30))
  
  return(w1, w2)
}


accessCloud('26 - 35', 'Very Satisfied')

#########################################
# Create function for barchart - Access #
#########################################
accessPlot = function(age, access_satisfaction, freq) {
  df_filter = df %>%
    filter(ageGroup == age) %>%
    filter(accessRating == access_satisfaction)
  
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
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot1)
}

# test function
accessPlot('26 - 35', 'Very Satisfied', 5)



###########################################
# Create function for wordcloud - Overall #
###########################################
overallCloud = function(age, overall_satisfaction) {
  df %>%
    filter(ageGroup == age) %>%
    filter(overallRating == overall_satisfaction)
  
  # extract comments
  comment = df$comments %>%
    str_replace_all(pattern = stopwords('english'), replacement = "")
  
  # tidy words
  commment_text = Corpus(VectorSource(comment))
  commment_text_clean = tm_map(commment_text, removePunctuation)
  commment_text_clean = tm_map(commment_text_clean, content_transformer(tolower))
  commment_text_clean = tm_map(commment_text_clean, removeNumbers)
  commment_text_clean = tm_map(commment_text_clean, stripWhitespace)
  commment_text_clean = tm_map(commment_text_clean, removeWords, stopwords('english'))
  
  w1 = wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 50, colors = rainbow(30))
  w2 = wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 70, colors = rainbow(30))
  
  return(w1, w2)
}


overallCloud('26 - 35', 'Very Satisfied')

#########################################
# Create function for barchart - Access #
#########################################
overallPlot = function(age, overall_satisfaction, freq) {
  df_filter = df %>%
    filter(ageGroup == age) %>%
    filter(overallRating == overall_satisfaction)
  
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
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot1)
}

# test function
overallPlot('16 - 25', 'Very Satisfied', 5)



################################################################################
#* function to create word chart and filter for i) access/overall satisfaction #
#* and ii) satisfaction level                                                  #
################################################################################

Plot2 = function(accessOrOverall, satisfaction, freq) {
  if (accessOrOverall == 'access'){
    df_filter = df %>%
      filter(accessRating == satisfaction)
    
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
      filter(overallRating == satisfaction)
    
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
      ggtitle(sprintf("Most common words from patient comments who rated\n overall satisfaction as '%s'", satisfaction)) +
      theme(plot.title = element_text(hjust = 0.5))
    
    return(plot1)
  }
}
##* look through different satisfaction by changing number in [] from 1 (very
##* disasisfied) to 5 (very satisfied) and access/overall satisfaction
Plot2('overall', levels(df$overallRating)[3], 30)
Plot2('overall', levels(df$overallRating)[1], 14)
