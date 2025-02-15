#Exploratory Data Analysis

#Load packages

library(data.table)
library(here)
library(tidyverse)

#Load data
  #Directory
if(!dir.exists("./data")){dir.create(here("./data"))}#Set-up directory for work and data
  #Download data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile() #Temporary variable to save zip file
download.file(url,temp)

  #Export data
unzip(temp, list = TRUE) #Explore zip folder
zip_list <- as.vector(unzip(temp, list = TRUE)[,1]) #Save structure
activity <- read.csv2(unzip(temp, zip_list[1]), sep=",")

unlink(temp); rm(temp, url, zip_list) #Delete temporary files

#Explore Data
head(activity)
str(activity)

  #Convert to right format
activity$date <- as.Date(as.character(activity$date), "%Y-%m-%d")

#Questions
#What is the mean total number of steps taken per day?
#Hist

activity <- activity %>%
  mutate(day = lubridate::wday(date, label = TRUE))

dailysteps <- activity %>%
  mutate(day = lubridate::wday(date, label = TRUE)) %>%
  group_by(day) %>%
  summarise(total_steps = sum(steps, na.rm=T))

meansteps <- activity %>%
  mutate(day = lubridate::wday(date, label = TRUE)) %>%
  group_by(day) %>%
  summarise(avg_steps = mean(steps, na.rm=T))

hist(meansteps$avg_steps)

meansteps <- mean(dailysteps$total_steps)
mediansteps <- median(dailysteps$total_steps)

#What is the average daily activity pattern?

dpattern <- activity %>%
  group_by(day, interval) %>%
  summarise(mean_steps = mean(steps, na.rm=T))

patternplot <- ggplot(dpattern, aes(dpattern$interval, dpattern$mean_steps, colour = mean_steps))

patternplot + 
  geom_line() +
  labs(title = "Daily Patterns",x="Intervals",y="Mean steps") +
  facet_wrap(.~day, ncol=1)

#Imputing missing values

sum(rowSums(is.na(activity)))

no_na_activity <- activity %>%
pivot_wider(names_from = interval, values_from = c(steps))

names(no_na_activity) <- make.names(names(no_na_activity))

for(i in 3:ncol(no_na_activity)) {
  no_na_activity[,i][is.na(no_na_activity[,i])] <- as.integer(colMeans(no_na_activity[,i],na.rm = TRUE))
}

no_na_activity <- pivot_longer(no_na_activity, 
                               cols = 3:290, 
                               names_to = "interval", 
                               values_to = "steps")
no_na_activity$interval <- as.numeric(gsub("X", "",as.character(no_na_activity$interval)))


hist(no_na_activity$steps)

no_na_meansteps <- no_na_activity %>%
  mutate(day = lubridate::wday(date, label = TRUE)) %>%
  group_by(day) %>%
  summarise(avg_steps = mean(steps, na.rm=T))


hist(no_na_meansteps$avg_steps)

no_na_dpattern <- no_na_activity %>%
  group_by(day, interval) %>%
  summarise(mean_steps = mean(steps, na.rm=T))

no_na_patternplot <- ggplot(no_na_dpattern, 
                            aes(no_na_dpattern$interval, 
                                no_na_dpattern$mean_steps, 
                                colour = mean_steps))

no_na_patternplot + 
  geom_line() +
  labs(title = "Daily Patterns",x="Intervals",y="Mean steps") +
  facet_wrap(.~day, ncol=1)

#Weekdays v Weekends

weeks <- no_na_activity %>% 
  mutate(weeks = case_when(
    day=="Sun" ~ "Weekend",
    day == "Sat" ~ "Weekend",
    TRUE ~ "Weekday"
  ))

weeks <- weeks %>% 
  group_by(weeks, interval) %>%
  summarise(avg_steps = mean(steps))

ggplot(weeks, aes(interval, avg_steps, colour = avg_steps)) + 
  geom_line() + 
  facet_wrap(~ weeks, scales = "free_y", ncol = 1) +
  ylab("Average Steps") + xlab("Interval")






