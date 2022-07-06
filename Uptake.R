library(tidyverse)
require(lubridate)
require(timetk)
require(rms)


########################
# import and tidy data #
########################

cons = read_csv('rawData/consultation.csv', skip = 6) %>%
  mutate(Date = dmy_hm(`Entry Time (local)`)) %>%
  mutate(month = month(Date, label = TRUE)) %>%
  mutate(year = year(Date)) %>%
  mutate(month = factor(month, levels = c('Jan', 'Feb', 'Mar','Apr', 'May',
                                          'Jun', 'Jul', 'Aug', 
                                          'Sep', 'Oct', 'Nov', 'Dec'))) %>%
  rename(answerTime = `Time To Answer (sec)`) %>%
  rename(abandon = `Is Call Abandoned`) %>%
  rename(rejoin = `# Rejoins`) %>%
  rename(waitingArea = `Waiting Area`)



  
cons %>%
  filter(year == 2020) %>%
  ggplot(aes(x=as.Date(`Date`))) +
  geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  main()

cons %>%
  filter(year == 2021) %>%
  ggplot(aes(x=as.Date(`Date`))) +
  geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b")

cons %>%
  filter(year == 2022) %>%
  ggplot(aes(x=as.Date(`Date`))) +
  geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b")



  cons %>%
  mutate(site = str_split_fixed(cons$waitingArea, '/', 2)[,1]) %>%
  mutate(department = str_split_fixed(cons$waitingArea, '/', 2)[,2]) %>%
  ggplot(aes(x=as.Date(`Date`))) +
  geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") + 
  facet_wrap(~ site)
  
  cons %>%
    mutate(site = 'Other') %>%
    mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
    subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
    mutate(site = factor(site, levels = c('AUH', 'BGH', 'LUH', 'RLH', 'Other'))) %>%
    ggplot(aes(x=as.Date(`Date`))) +
    geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
    scale_x_date(date_breaks = "months", date_labels = "%b") + 
    geom_vline(xintercept = 10000, linetype="dotted", 
               color = "blue", size=1.5) +
    facet_wrap(~ site) +
    theme(axis.text.x = element_text(size=8, angle=45))
    

  cons %>%
    mutate(site = 'Other') %>%
    mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
    subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
    mutate(site = factor(site, levels = c('AUH', 'BGH', 'LUH', 'RLH', 'Other'))) %>%
    ggplot(aes(x=as.Date(`Date`))) +
    geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
    scale_x_date(date_breaks = "months", date_labels = "%b") + 
    geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
    theme(axis.text.x = element_text(size=8, angle=45)) + 
    ggtitle("Patient uptake of video consultation during 2020-2022") +
    ylab("patient uptake (count per 7 days)") +
    xlab("Time (Month)")
  
  cons %>%
    mutate(site = 'Other') %>%
    mutate(site = if_else(str_detect(waitingArea, 'AUH') == TRUE, 'AUH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'BGH') == TRUE, 'BGH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'RLH') == TRUE, 'RLH', site)) %>%
    mutate(site = if_else(str_detect(waitingArea, 'LUH') == TRUE, 'LUH', site)) %>%
    subset(site %in% c('AUH', 'BGH', 'LUH', 'RLH', 'Other')) %>%
    mutate(site = factor(site, levels = c('AUH', 'BGH', 'LUH', 'RLH', 'Other'))) %>%
    ggplot(aes(x=as.Date(`Date`))) +
    geom_line(aes(fill=..count..), stat="bin", binwidth = 7) + 
    scale_x_date(date_breaks = "months", date_labels = "%b") + 
    geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01"))), linetype=4) +
    theme(axis.text.x = element_text(size=8, angle=45)) + 
    facet_wrap(~ site) +
    ggtitle("Patient uptake of video consultation during 2020-2022 per site") +
    ylab("patient uptake (count per 7 days)") +
    xlab("Time (Month)")

  
  
  cons %>%
    mutate(Speciality = str_split_fixed(cons$waitingArea, '/', 2)[,2]) %>%
    ggplot(aes(x=as.Date(`Date`), fill = Speciality)) +
    geom_bar(aes(fill=..count..), stat="bin", binwidth = 7)

