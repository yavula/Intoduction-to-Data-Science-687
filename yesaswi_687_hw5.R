################################################
# IST 687, HW05
#
# Student name: Yesaswi Avula
# Homework number: HW05
# Date due: 09/30/2019
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work with help from the book and the professor and these Internet sources: 
# <https://www.rdocumentation.org/packages/stringr/versions/1.4.0/topics/str_trim>
# <https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/filter>
# <https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/tapply>
# <https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/pull>
# <>

# Run these three functions to get a clean test of homework code
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

install.packages("tidyverse")
install.packages("RCurl")
install.packages("jsonlite")
install.packages("RJSONIO")
install.packages("dplyr")

library(RCurl)
library(RJSONIO)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(stringr)
# Downloading a JSON containing Maryland accident data and storing it as a dataframe
dataset <- getURL("http://opendata.maryland.gov/resource/pdvh-tf2u.json")
df <- jsonlite::fromJSON(dataset)
View(df) # Viewing the Maryland accidents datasets
str(df)

# Coercing the numeric string of vehicle count attribute into a numeric value
df$vehicle_count <- as.numeric(df$vehicle_count)

 # Using tidyverse piping we are trimming the day_of_week attribute to ensure errors aren't
# caused due to white spaces before or after the value. Then we are extracting the 
# the instances with the day of the week as THURSDAY using the filter(). Then we are extracting the 
# vehicle count from the subset of data instances with day of the week as THURSDAY.
# Lastly we are calculating the mean of the vehicle count where the day of the week is 
# THURSDAY and we are ignoring the instances of vehicle count containing the "na" value.
# The calculate mean is stored in the "value" variable
value <- df %>%
  filter(str_trim(day_of_week)=="THURSDAY") %>%
  pull(vehicle_count) %>%
  mean(na.rm=TRUE)
value # Mean of vehicle_count with the day_of_week as thursday ignoring "na" values

# Computing the same mean as above except we are trying to achieve it without using tidyverse's piping 
value <- mean( df$vehicle_count[str_trim(df$day_of_week)=="THURSDAY"], na.rm=TRUE)
value # Mean of vehicle_count with the day_of_week as thursday ignoring "na" values

# Checking which attributes have na values
which(is.na(df$case_number))
which(is.na(df$barrack)) # has na
which(is.na(df$acc_date))
which(is.na(df$acc_time))
which(is.na(df$acc_time_code))
which(is.na(df$day_of_week))
which(is.na(df$road))
which(is.na(df$intersect_road))
which(is.na(df$dist_from_intersect)) # has na
which(is.na(df$dist_direction)) # has na
which(is.na(df$city_name)) # has na
which(is.na(df$county_code)) # has na
which(is.na(df$county_name)) # has na
which(is.na(df$vehicle_count)) # has na
which(is.na(df$prop_dest))
which(is.na(df$injury))
which(is.na(df$collision_with_1))
which(is.na(df$collision_with_2))

# Step 2A
# Total number of accidents with injuries
acc_inj <- df %>%
  filter(str_trim(injury)=="YES") %>%
  pull(case_number) %>%
  length()
acc_inj

# Step 2B
# Total accidents on friday
acc_fri <- df %>%
  filter(str_trim(day_of_week)=="FRIDAY") %>%
  pull(case_number) %>%
  length()
acc_fri

# Step 2C
# Total accidents with injuries on a friday
acc_fri_inj <- df %>%
  filter(str_trim(day_of_week)=="FRIDAY") %>%
  filter(str_trim(injury)=="YES") %>%
  pull(case_number) %>%
  length()
acc_fri_inj

# Step 2D
# Total accidents without injuries on a friday
acc_fri_no_inj <- df %>%
  filter(str_trim(day_of_week)=="FRIDAY") %>%
  filter(str_trim(injury)=="NO") %>%
  pull(case_number) %>%
  length()
acc_fri_no_inj

# acc_day<-tapply(df$case_number, df$day_of_week, length)
# acc_day

# Step 2E
# Total accidents with injuries each day of the week

# acc_day <- df %>%
#   filter(str_trim(injury)=="YES") %>%
#   group_by(day_of_week) %>%
#   summarise(count=n())
# acc_day

acc_day <- df %>%
  filter(str_trim(injury)=="YES") %>%
  group_by(day_of_week) %>%
  length()
acc_day

# Step 2F
# Dataframe with accidents on friday
acc_fri_df <- df %>%
  filter(str_trim(day_of_week)=="FRIDAY")
View(acc_fri_df)

# Step 2G
# Mean number of vehicles in accidents on friday
fri_mean_veh <- acc_fri_df %>%
  pull(vehicle_count) %>%
  mean(na.rm=TRUE)
fri_mean_veh

# Step 2H
# histogram for number of cars in accidents on friday
hist(acc_fri_df$vehicle_count,xlab = "Distribution of number of vehicles involved in an accident on a friday")

# Step 2I
# Number of vehicles in accidents on Sunday
acc_sun <- df %>%
  filter(str_trim(day_of_week)=="SUNDAY")

hist(acc_sun$vehicle_count)

# Spotting 5% and 95% quantile
q1_sun <- quantile(acc_sun$vehicle_count, 0.05, na.rm = TRUE)
q2_sun <- quantile(acc_sun$vehicle_count, 0.95, na.rm = TRUE)
q_sun <- c(q1_sun,q2_sun)
q_sun

# Comparison between number of accidents on friday and sunday.
# There is a higher tendency that an accident involves two cars on friday where as 
# the number of cars involved in an accident is one on a sunday. 
