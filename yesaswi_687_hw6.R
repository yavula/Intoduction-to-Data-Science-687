################################################

# IST687, Standard Homework Heading
#
# Student name:Yesaswi Avula
# Homework number:PE & HW 6
# Date due: 
#
# Attribution statement: (choose the one statement that is true)

# 1. I did this homework by myself, with help from the book and 
# the professor

# 2. I did this homework with help from the book and the 
# professor and these Internet sources: <provide the urls>

# 3. I did this homework with coaching from <Name of another 
# student> but did not cut and paste any code

#dev.off() # Clear the graph window
#cat('\014')  # Clear the console
#rm(list=ls()) # Clear all user objects from the environment!!!

# Set working directory 
# Change to the folder containing your homework data files
#setwd("~/GoogleDrive-SYR/Fall 2019/HW")
# PE 6

# 1.	For this exercise you will answer all of the questions
#  in this document and turn it in to Blackboard.

# 2.	Before you get started make sure to read Chapters 12 of An 
# Introduction to Data Science and execute the code throughout the
# chapter to gain familiarity.  

# 3.	Getting Started: Data visualization is important because 
# many people can make sense of data more easily when it is presented
# in graphic form. As a data scientist, you will have to present
# complex data to decision makers in a form that makes the data
# interpretable for them. From your experience with Excel and other 
# tools, you know that there are a variety of common data 
# visualizations (e.g., pie charts). How many of them can you name? 

# Bar plots, histagram, pie charts, scatter, line chart, waterfall, heat map, bubble chart
# box and whisker, 

#As usual we'll use the Prep Ex to clean up our data and 
# this time, we will also merge two datasets, using the built-in merge( )
# function, which provides a similar capability to a JOIN in SQL. Many 
# analytical strategies require joining data from different sources based
# on a "key" - a field that two datasets have in common. Specifically, 
# we'll revisit the USArrests dataframe that is built-in to R as well as the census dataset. 
getwd()
# Run your Readstates function from the last HW to get our census dataset I called it "states"
readStates <- function(){
  
  #data <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  data <- "Nst-est2011.csv"
  dfStates <- read.csv(data, stringsAsFactors = FALSE)
  
  # clean the dataset
  dfStates<-dfStates[-1:-8,]
  dfStates<-dfStates[,1:5]
  dfStates<-dfStates[-52:-58,]
  
  #define column Names
  cnames <- c("stateName", "Census",  "Estimated", "Pop2010", "Pop2011")
  colnames(dfStates) <- cnames
  
  # remove dots  
  dfStates$stateName <- gsub("\\.","", dfStates$stateName)
  
  # remove commas
  dfStates$Census <-gsub(",", "", dfStates$Census)
  dfStates$Estimated <-gsub(",", "", dfStates$Estimated)
  dfStates$Pop2010 <- gsub(",", "", dfStates$Pop2010)
  dfStates$Pop2011 <- gsub(",", "", dfStates$Pop2011)
  
  # remove spaces and convert to numeric type
  dfStates$Census <-as.numeric(gsub("", "", dfStates$Census))
  dfStates$Estimated <-as.numeric(gsub("", "", dfStates$Estimated))
  dfStates$Pop2010 <- as.numeric(gsub("", "", dfStates$Pop2010))
  dfStates$Pop2011 <- as.numeric(gsub("", "", dfStates$Pop2011))
  return(dfStates)
}

# Step 1:  Use the merge command to create a new dataframe
# A.	Code and Execute the following block of code. Be sure to understand
# each line. As a reminder, you created the 'readStates' function in a 
# previous homework assignment, so you should be able to reuse that code. 
# Add comments before each line to explain in detail what each line of code does.

states <- readStates() # calling readStates function to get and clean my census dataset, soring it in a dataframe called states 
arrests <- USArrests  # creatign a data set from the in R dataset called USArrests
View(arrests)  # look at arrests before new column
arrests$stateName <- rownames(arrests)  # Creating a new column called statename because the statenames in the arrests data set are rownames, not a column
View(arrests)  # look at arrests after new column
View(states)
# Create a merged dataset called mergeDF.  This command needs there to be a similar
# column name in each dataset to know how to merge them. In this case 
# statename was used.
mergeDF <- merge(states, arrests, by = "stateName")
View(mergeDF)  # Look at your new merged dataset, should have 9 columns

# We'll also start to use one of the most powerful tools for data visualization
# in R -  ggplot. Written by computer/data scientist Hadley Wickham, this "graphics
# grammar" tool builds visualizations in layers. This method provides immense 
# flexibility, but takes a bit of practice to master. But first we'll be cleaning up our data.  

# Step 2:  Use ggplot to start to explore our merged dataframe
# A.	Install and library the ggplot2 package.

install.packages("ggplot2")
library(ggplot2)  # checking in the ggplot2 package

#B.	Code and Execute the following block of code (actually type, do not copy/paste).
# Add comments before each line to explain in detail what each line of code does. Add 
# an appropriate title for the chart (using 'ggtitle')

# Executing ggplot, need 3 things, data, asthetics, geometry
ggplot(mergeDF) +  # executing ggplot function using mergeDF data set
  aes(y=Murder ) +  #adding the aesthetics, or the colum of data to plot
  geom_boxplot() +  # Telling the function what type of visualization to use
  ggtitle("Boxplot of distribution of Murder Rate")  # Adding a title to my visualization

# C.	Cut and paste an image of the visualization created by the ggplot and explain what you see
# need visualization
# you see box and whiskers where the box has a line in it representing the median, Box width is the upper and 
# lower quartiles (so the inter quartile range), and the whiskers rach the high and low values.

# D.	Code and Execute the following block of code (actually type, do not copy/paste).
# Add comments before each line to explain in detail what each line of code does. Add an appropriate
# title for the chart (using 'ggtitle')

myPlot <- ggplot(mergeDF, aes(x=Murder))  # telling function my data and aestheics
# telling what type of visualization, plus adding color of line and fill, as well 
# as my binwidth.  Notice I add to my previous plot "myplot"
myPlot <- myPlot + geom_histogram(binwidth=2, color="black", fill="white")  

myPlot <- myPlot + ggtitle("Histogram of murder rates")  # Adding title to visualization
myPlot  # displaying my final visualization

# E.	Cut and paste an image of the visualization created by the ggplot and explain what you see

# We see a bor plot, with boxs of width 2, filled with White color and bordered in black color with our title

# 1.	List any additional resources you used here. 


# 2.	Be sure to save your R file as this will become the starting code for your homework. 


# ******************************** HW 6  *******************************************************
readStates <- function(){
  
  #data <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv"
  data <- "nst-est2011-01.csv"
  dfStates <- read.csv(data, stringsAsFactors = FALSE)
  
  # clean the dataset
  dfStates<-dfStates[-1:-8,]
  dfStates<-dfStates[,1:5]
  dfStates<-dfStates[-52:-58,]
  
  #define column Names
  cnames <- c("stateName", "Census",  "Estimated", "Pop2010", "Pop2011")
  colnames(dfStates) <- cnames
  
  # remove dots  
  dfStates$stateName <- gsub("\\.","", dfStates$stateName)
  
  # remove commas
  dfStates$Census <-gsub(",", "", dfStates$Census)
  dfStates$Estimated <-gsub(",", "", dfStates$Estimated)
  dfStates$Pop2010 <- gsub(",", "", dfStates$Pop2010)
  dfStates$Pop2011 <- gsub(",", "", dfStates$Pop2011)
  
  # remove spaces and convert to numeric type
  dfStates$Census <-as.numeric(gsub("", "", dfStates$Census))
  dfStates$Estimated <-as.numeric(gsub("", "", dfStates$Estimated))
  dfStates$Pop2010 <- as.numeric(gsub("", "", dfStates$Pop2010))
  dfStates$Pop2011 <- as.numeric(gsub("", "", dfStates$Pop2011))
  return(dfStates)
}

#Step 1: Explore the Merged Data � Understanding distributions
#A.	Use the merged dataset created during this weeks prep work (mergedDF)

states <- readStates() # calling readStates function to get and clean my census dataset, soring it in a dataframe called states 
arrests <- USArrests  # creatign a data set from the in R dataset called USArrests
View(arrests)  # look at arrests before new column
arrests$stateName <- rownames(arrests)  # Creating a new column called statename because the statenames in the arrests data set are rownames, not a column
View(arrests)  # look at arrests after new column
View(states)
# Create a merged dataset called mergeDF.  This command needs there to be a similar
# column name in each dataset to know how to merge them. In this case 
# statename was used.
mergeDF <- merge(states, arrests, by = "stateName")
View(mergeDF)  # Look at your new merged dataset, should have 9 columns

#B.	Create separate histograms using ggplot2() for the population, murder rate, assault and rape columns. 
#Make sure each line of code is explained (comments) in terms of what it is doing.
#What parameter will you have to adjust to make the other histograms look right?

# Create Murder plot
ggplot(mergeDF) + aes(x=Murder) +               # Defining data frame and column
  geom_histogram(binwidth=2, color="black", fill="white")  +     # telling to create a histogram and other detais, have to adjust binwidth to make all equal
  ggtitle("Histogram of murder rates")      # Adding hitsogram title

# Create Rape plot
ggplot(mergeDF, aes(x=Rape)) +                   # Defining data frame and column
  geom_histogram(binwidth=5, color="black", fill="white")  +    # telling to create a histogram and other detais, have to adjust binwidth to make all equal
  ggtitle("Histogram of assault rates")                 # Adding hitsogram title

# Create Assault plot
ggplot(mergeDF, aes(x=Assault)) +                 # Defining data frame and column
  geom_histogram(binwidth=10, color="black", fill="white")  +   # telling to create a histogram and other detais, have to adjust binwidth to make all equal
  ggtitle("Histogram of assault rates")           # Adding hitsogram title

# Create Pop2011 plot
ggplot(mergeDF, aes(x=Pop2011)) +           # Defining data frame and column
  geom_histogram(binwidth=1000000, color="black", fill="white")  +    # telling to create a histogram and other detais, have to adjust binwidth to make all equal
  ggtitle("Histogram of assault rates")                # Adding hitsogram title

#C.	Create a boxplot for the population 2011, and a different boxplot for the murder rate.
ggplot(mergeDF) + 
  aes(y=Murder ) + 
  geom_boxplot() +
  ggtitle("boxplot of distribution of Murder Rate")

ggplot(mergeDF) + 
  aes(y=Pop2011 ) + 
  geom_boxplot() +
  ggtitle("boxplot of distribution of Pop2011")

#D.	Create a block comment explaining which visualization (boxplot or histogram) 
#   you thought was more helpful (explain why)
#I like histograms!!! More detial, see trends,

#Step 2: Which State had the Most Murders using bar charts
#A.	Calculate the number of murders per state
#a.	Hint: use the population and murder rate percentage from your new dataframe
mergeDF$numMurders <- (mergeDF$Pop2011 * mergeDF$Murder)/100000
View(mergeDF)

# Install the needed package
install.packages("ggplot2")
library(ggplot2)  # checking in the ggplot2 package

#B.	Generate a bar chart, with the number of murders per state
#Hint: use the geom_col() function
ggplot(mergeDF) +
  aes(x=stateName, y=numMurders) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Murders")

#C.	Generate a bar chart, with the number of murders per state. Rotate text (on the X axis), so we can see x labels, also add a title named “Total Murders”.
#a.	Hint: use theme(axis.text.x = element_text(angle = 90, hjust = 1))
# --> same as above

#D.	 Generate a new bar chart, the same as in the previous step, 
#    but also sort the x-axis by the murder rate from low to high
ggplot(mergeDF) +
  aes(x=reorder(stateName, numMurders), y=numMurders) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Total Murders")

#E.	 Generate a third bar chart, the same as the previous step, 
#    but also showing UrbanPop as the color of the bar
ggplot(mergeDF) +
  aes(x=reorder(stateName, numMurders), y=numMurders, fill=UrbanPop) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("state") +  ylab("Murder rate") + 
  ggtitle("Murder Rates and Population by State")

#    Step 3: Explore Murders using scatter chart
#A.	 Generate a scatter plot and have Pop2011 on the X axis, 
#    the UrbanPop on the y axis, and the size & color represent the number of murders. 
ggplot(mergeDF) +
  aes(x=Pop2011, y=UrbanPop) + 
  geom_point(aes(size = numMurders, color=numMurders)) + 
  scale_color_gradient(low="white", high="red")




