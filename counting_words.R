####################
# install packages #
####################

require(tidytext)
require(tm)
require(wordcloud)
library(tidytext) # For un                                                                                                                                                                                       nest_tokens
library(stringr) # For managing text

#########################
# import data  and tidy #
#########################

df = read_csv('rawData/raw_data.csv')

# preliminary tidy
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



##############################################
# Create frequency histogram of common words #
##############################################

# extract comments
comment = df$comments

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
  filter(n > 100) %>% # filter freq accordingly 
  filter(word != 'na') %>%
  mutate(word = reorder(word,n))
  
  ggplot(data = text_wordcounts, aes(word, n)) +
  geom_col(fill= 'darkred') +
  coord_flip() +
  geom_text(aes(label = n), hjust = 1.2, colour = 'white', fontface = 'bold') +
  theme(plot.title = element_text(hjust = 0.5))
  
  
#######################
# Create a word cloud #
#######################

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

wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 50, colors = rainbow(30))
wordcloud(commment_text_clean, scale = c(2, 1), min.freq = 70, colors = rainbow(30))


