#Martians Assignment 4
#Breanna Vasko

library(readr)
library(dplyr)
library(tidyverse)
library(datasets)

#Reading the data into a data frame
ogdata <- read_csv("ufo_subset.csv")

#Pipe lining the original data to follow the specific assignment tasks:
ufodf <- ogdata %>% 
  mutate(datetime=as.Date(datetime, format = "%d.%m.%Y")) %>% #Formatting datetime to be the same structure as date.posted
  drop_na(country, shape) %>%  #Removing NA values to clean the data
  #CODE REVIEW COMMENT: Viable strategy for eliminiating hoaxes! I'm not sure if all of the ones mentioned as HOAX are offiical NUFORC though.
  filter(!grepl("HOAX", comments, ignore.case = T)) %>% #Filter out hoax sightings in the comments column
  
  #' CODE REVIEW COMMENT 2: another way to do find report_delays is to convert the date columns into POSIX and then use the difftime function:
  #' 1. use the as.POSIX function
  #' 2. or use lubridate ymd_hms/ymd functions 
  #' This would be important if the dates had different formats for each column
  #' --------------------------------------------------------------------------
  mutate(report_delays = date.posted - datetime) %>% #Adding a new column with difference in report and posting in days
  
  filter(!grepl("-", report_delays)) %>% #Filter out the rows where the sighting was reported before it happened
  group_by(country) %>% #Grouped by country for table in next step
  summarise(mean(report_delays)) #Create a table with the average report_delay per country

#Check the data quality (missingness, format, range etc) of the duration(seconds) column
#The duration column has extremely large values, so to fix the data quality a more appropriate range should be used.
# There a no missing values so removing NA's is not necessary 

#CODE REVIEW COMMENT: why did you choose this range?
ufodurations <- ogdata %>%
  filter(duration..seconds. <= 6312000) #Setting a max limit for the duration

#CODE REVIEW COMMENT: ufodf2 was not found, changed to ufodf
#Creating a histogram with the duration(s) columns from cleaned data 
hist(log10(ufodf$duration..seconds.), main = "Duration(s)", xlab = "log of Duration")
