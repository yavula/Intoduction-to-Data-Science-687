################################################

# IST687, Standard Homework Heading

#



#

# Attribution statement: (choose the one statement that is true)

# 1. I did this homework by myself, with help from the book and 

# the professor


# Run these three functions to get a clean test of homework code

#dev.off() # Clear the graph window

#cat('\014')  # Clear the console

#rm(list=ls()) # Clear all user objects from the environment!!!



# Set working directory 

# Change to the folder containing your homework data files

#setwd("//hd.ad.syr.edu/02/5c3dde/Documents/Desktop")

# PE 7

#1.	Getting Ready: Load and repair median income data
# A.	Download the provided MedianZIP.csv file from Blackboard and read
# into R-studio into a dataframe called “mydata”. HINT: Use read_csv() to simplify later steps!

# Install needed Packages for read_csv function
getwd(
install.packages("tidyverse")
library(tidyverse)  # Need this for the read_csv() function

#Load the data set needed for PE
mydata <- read_csv("MedianZIP.csv")

str(mydata)  # Understand your data
View(mydata)  # Understand your data

#mydata$Mean <- as.numeric(mydata$Mean)  # Convert Mean data column from Char tonumeric

# B.	Cleaning up the NAs: Find and fix the missing data in the Mean column by substituting the value from the Median column in place of the missing mean values. 
# Explain why the median is a reasonable replacement for the mean.
#Validates there are 7 na's in Mean
str(mydata)  # see the structure of your data

sum(is.na(mydata$Mean))  # see how many na values there it  (7)

mydata$Mean[is.na(mydata$Mean)] <- mydata$Median[is.na(mydata$Mean)]  # Replace the na's with the median if NA

sum(is.na(mydata$Mean))  # see how many na values there it  (0) now since you replaced with the median

# C.	Examine the data with View( ) and add comments explaining what each column contains. 
# Add a comment explaining why the first 2391 zip codes look weird. 
View(mydata)   #Look at zip column, see missing zero's
# Comment, we have 4 digit zip codes, they are missing the leading zero's
# They need to be cleaned using the zipcode package

# 2.	Merge the median income data with detailed zipcode data
# A.	Code and execute the commands below. Write a paragraph below 
# that explains what this code does.
# The zipcode package was removed on 1/1/2020, this is an old archive install from the archive

install.packages("~/Desktop/zipcode_1.0.tar.gz", repos = NULL, type = "source")  # Installs new package zipcode to be used to clean zip codes
library(zipcode)  # Check in the pacakge

mydata$zip <- clean.zipcodes(mydata$zip)  # code will clean the zip codes and store good value in zip column.  Will make it a character
data(zipcode)  #loads the specified data set
str(zipcode)  # Get to know your data, notice the column zip is character

str(mydata)  # Notice zip is in both files and both have same data type
dfNew <- merge(mydata, zipcode, by="zip")  # merges the mydate and zipcode data sets by "zip"

View(mydata) # confirm you replaced the leading zeros in zipcode

# 3.	Merge the new dataset with stateNameDF data
# A.	Create a new dataframe with the following code:
# stateNameDF <- data.frame(state=state.abb, stateName=state.name, center=state.center)
# stateNameDF$stateName <- tolower(stateNameDF$stateName)
#use some built in data to create a new dataframe
stateNameDF <- data.frame(state=state.abb, stateName=state.name, center=state.center)


# C.	Using steps similar to step 2 create a new dataframe that contains 
# our previous information and the information from the stateNameDF.  
#convert statename to lower case
stateNameDF$stateName <- tolower(stateNameDF$stateName)

# Now merge the dfNew data set from before with your statename data set
dfNew <- merge(dfNew, stateNameDF, by="state")
sum(is.na(dfNew$Mean))
# 4.	Examine your new df with the View command. I
#  A.	Include a screen shot of the first 10 rows of data and all of your columns. 
head(dfNew,10)
#stateName center.x center.y
#1     alaska  -127.25    49.25
#2     alaska  -127.25    49.25
#3     alaska  -127.25    49.25
#4     alaska  -127.25    49.25
#5     alaska  -127.25    49.25
#6     alaska  -127.25    49.25
#7     alaska  -127.25    49.25
#8     alaska  -127.25    49.25
#9     alaska  -127.25    49.25
#10    alaska  -127.25    49.25

#****************************************  HW 07 *********************

#Step 1:  Plot points for each zipcode (don’t forget to library ggplot2 and ggmap)
#A.	Code and execute the following lines of code
install.packages("maps")
library(maps)

us <- map_data("state")
View(dfNew)
dotmap<- ggplot(dfNew, aes(map_id = stateName))
dotmap<- dotmap + geom_map(map = us)
dotmap<- dotmap + geom_point(aes(x=longitude,y=latitude,color=Mean))
dotmap

#B.	Comment each line of code explaining what it does.

#C.	Add a block comment that criticizes the resulting map. 
# It’s not very good (but you need to explain why it is not very good).
# too many dots...

# Step 2:  Use Tidyverse to create a Data Frame of State-by-State Income
#A.	Library the tidyverse() package (if you have not already done so), and then run the following command to create a new data frame:
summaryDF <- dfNew %>%
  group_by(stateName) %>%
  summarize(totalPop=sum(Pop), Income=sum(Mean*Pop))

sum(is.na(summaryDF$Income))   #3 NA's  California, Texas and Virginia
#B.	Add a comment after each line, describing what each line of code does. Make sure to describe how many rows there are in the new dataframe, and how the new dataframe was created.

#C.	Create a new variable on this data frame called meanIncome. Divide Pop by Income to find the average income for each state.
summaryDF$meanIncome <- summaryDF$Income/summaryDF$totalPop
sum(is.na(summaryDF$meanIncome))
sum(is.na(summaryDF$Income))
#summaryDF <- summaryDF %>%
#  mutate(meanIncome=Income/totalPop)
View(summaryDF)

#Step 3:  Create a map of the U.S. showing average income
#A.	Create a map visualization, where the color of each state represents the mean income for that state.
#B.	If you need help creating the map, review Chapter 13, and how Figure 13.2 was created.

# Install needed package for expand limits
install.packages("mapproj")
library(mapproj)

View(summaryDF)
ggplot(summaryDF) +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income")

ggplot(summaryDF) +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income")
#C.	You will notice some states are grey (they had NAs). So, remove NAs (any way you think is appropriate) and then generate the map visualization, where the color of each state represents the mean income for that state.

#remove NAs -  using an approach that makes the most sense
#only 6 out of 32K, so just remove
dfNew <- dfNew[complete.cases(dfNew),]

summaryDF <- dfNew %>%
  group_by(stateName) %>%
  summarize(totalPop=sum(Pop), Income=sum(Mean*Pop))
summaryDF$meanIncome <- summaryDF$Income/summaryDF$totalPop

ggplot(summaryDF) +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income")

#or entire data munge and vis in one statement
dfNew %>%
  group_by(stateName) %>%
  summarize(totalPop=sum(Pop), 
            Income=sum(Mean*Pop), 
            meanIncome=Income/totalPop) %>%
  ggplot() +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income")


#Step 4:  Show the population of each state on the map
#B.	Create a new map visualization, which is the same visualization created in step 3, 
#   but now add a point, at the center of each state, 
#   and the point size represents the total population of that state.
#C.	If you need a hint on how to do this visualization, 
#    you need to have a center.x and center.y in your summaryDF 
#    (i.e., you need to create a new summaryDF with center.x and center.y).


#make sure have initial 
View(stateNameDF)
summaryDF <- merge(summaryDF, stateNameDF, by="stateName")

ggplot(summaryDF) +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  geom_point(aes(x=center.x, y=center.y, size=totalPop)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income") 

#or one big statement
dfNew %>%
  group_by(stateName) %>%
  summarize(totalPop=sum(Pop), 
            Income=sum(Mean*Pop), 
            meanIncome=Income/totalPop,
            center.x=mean(center.x),
            center.y=mean(center.y)) %>%
  ggplot() +
  aes(map_id = stateName) +
  geom_map(map = us, aes(fill=meanIncome)) +
  geom_point(aes(x=center.x, y=center.y, size=totalPop)) +
  expand_limits(x = us$long, y = us$lat) + 
  coord_map() + ggtitle("state income") 


