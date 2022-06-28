library(tidyverse)

cons = read_csv('rawData/consultation.csv', skip = 6)

disat = filter(df, accessRating.coded < 3)

colnames(df)
