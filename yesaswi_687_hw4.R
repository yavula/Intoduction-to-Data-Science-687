################################################
# IST 387/687, Standard Homework Heading
#
# Student name: Yesaswi Avula
# Homework number: 4
# 
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work with help from the book and the professor 
#    and these Internet sources: <provide the urls>

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Printing Numeric Vector Information/Summary Statistics
printVecInfo <- function(numVector){
  
  vec_mean <- mean(numVector) # Mean of the input vector
  vec_median <- median(numVector) # Median of the input vector
  vec_min <- min(numVector) # Minimum value of the input vector
  vec_max <- max(numVector) # Maximum value of the input vector
  vec_q1 <- quantile(numVector, probs = 0.05) # 0.05 Quantile of the vector
  vec_q2 <- quantile(numVector, probs = 0.95) # 0.95 Quantile of the vector
  
  print(vec_mean) # Printing vector mean
  print(vec_median) # Printing vector median
  print(vec_min) # Printing vector minimum value
  print(vec_max) # Printing vector maximum value
  print(vec_q1) # Printing vector 0.05 quantile
  print(vec_q2) # Printing vector 0.95 quantile
  
  cat("Vector Mean: ", vec_mean, "\n") # Printing vector mean
  cat("Vector Median: ", vec_median, "\n") # Printing vector median
  cat("Vector Minimum: ", vec_min, "\n") # Printing vector minimum value
  cat("Vector Maximum: ", vec_max, "\n") # Printing vector maximum value
  cat("Vector Quantile 0.05: ", vec_q1, "\n") # Printing vector 0.05 quantile
  cat("Vector Quantile 0.95: ", vec_q2, "\n") # Printing vector 0.95 quantile
}

# install.packages("imputeTS") # Installing the imputeTS package
library(imputeTS)

# Creating a DF with the NYC airquality data
myAQdata <- airquality
View(myAQdata) # Viewing the air quality dataset

# The "is.na(myAQdata$Ozone)" generates a boolean vector stating whether a specific 
# row contains "na" or not. If "na" is present then that row is assigned a TRUE value
# in the vector or else a FALSE value is assigned. The length of the generated vector
# is the length of the dataframe column. 
# Printing all the "na" values in the Ozone column of the air quality dataset.
myAQdata$Ozone[is.na(myAQdata$Ozone)] 
na_index_ozone <- is.na(myAQdata$Ozone)

# Replacing the "na" values with the mean of the remaining ozone data
ozone_mean <- mean(myAQdata$Ozone, na.rm=TRUE) # Calculating ozone mean without considering the "na" values
ozone_mean
myAQdata$Ozone[na_index_ozone] <- ozone_mean # Replace "na" with ozone mean
myAQdata$Ozone # Printing ozone values

# Replacing the "na" values with the mean of the remaining Solar.R data 
na_index_solar <- is.na(myAQdata$Solar.R)
solar_mean <- mean(myAQdata$Solar.R, na.rm = TRUE) # Calculating Solar.R mean without "na"
myAQdata$Solar.R[na_index_solar] <- solar_mean # Replace "na" with Solar.R mean
myAQdata$Solar.R # Printing solar values

# All the columns containing "na" values have been dealt
View(myAQdata) # Viewing the clean air quality data

# Loading air quality data into a new DF
myAQdata1 <- airquality
View(myAQdata1) # Viewing the original/unmodified air quality data

# Replacing the ozone, solar columns' "na" values with interpolation values
myAQdata1$Ozone <- na_interpolation(myAQdata1$Ozone)
myAQdata1$Solar.R <- na_interpolation(myAQdata1$Solar.R)
View(myAQdata1)

# The fifth row  used to contain "na" values in both the Ozone and Solar.R attributes
# The myAQdata and myAQdata1 have the cleaned dataset where the "na" values have
# been replaced. 
# myAQdata has the "na" data replaced with the mean i.e. with 42.12931 for Ozone attribute
# and with 185.9315 for the Solar.R attribute.
# myAQdata1 has the "na" data replaced with the interpolation values. The interpolation values
# are computed by taking the MEAN of the values one row ABOVE and BELOW. If there are 
# consecutive "na" values then the interpolation function takes the next available value for the
# computation of the value.
# Comparing the values in row 5 of both DF's shows that Ozone values are 42.12931 in myAQdata and 
# 23.0000 in myAQdata1. The Solar.R values are 185.9315 in myAQdata and 308.3333. 
# Since there are many "na" values it is probably not the best approach to replace with the
# overall MEAN. Interpolation might be a better approach.

# Sampling the wind column from the myAQData
sample1 <- sample(myAQdata$Wind, size = 10,replace = TRUE)
printVecInfo(sample1)
hist(sample1) # Histogram of the sample

# Replace is set to TRUE because it is essential that we maintain the original dataset
# If we don't replace it then we change the dataset that we draw from for the next
# samples. Hence the sampling result analysis won't be consistent with what we are expecting.
# That is the convergence of the mean of mean of sample to converge to true mean.
# Hence we replace the data after we have sampled it.

# Second Wind Sample
sample2 <- sample(myAQdata$Wind, size = 10, replace = TRUE)
printVecInfo(sample2)
hist(sample2)

# Third Wind Sample
sample3 <- sample(myAQdata$Wind, size = 10, replace = TRUE)
printVecInfo(sample3)
hist(sample3)

# Fourth Wind Sample
sample4 <- sample(myAQdata$Wind, size = 10, replace = TRUE)
printVecInfo(sample4)
hist(sample4)

# The sample in each of the sample's of Wind are different because the sample() function randomly
# selects the elements of the sample. This can be observed through the Histograms of the sample and 
# the vector information.

# Calculating the mean of the sample and replicating the sampling process 200 times
replicate1 <- replicate(200, mean(sample(myAQdata$Wind, size = 10, replace = TRUE)), simplify = TRUE)
hist(replicate1) # histogram of mean values of 200 samples of the Wind attribute 

# Repeating the replication step 
replicate2 <- replicate(200, mean(sample(myAQdata$Wind, size = 10, replace = TRUE)), simplify = TRUE)
hist(replicate2) # Histogram of the replication result

# Repeating the replication step
replicate3 <- replicate(200, mean(sample(myAQdata$Wind, size = 10, replace = TRUE)), simplify = TRUE)
hist(replicate3) # Histogram of the replication result

# The histograms generated by replicate1, replicate2, replicate3 are different
# because the replicate function generates a different sample each time, the sampling is replicated 
# 200 times and stored in one of the variables and the process is repeated 2 more times and results are
# stored in other variables. Since the sampling function is random the results tend to be random.
# However we can observe that the shape of the histograms in all the cases are similar to a bell shape.
# This tells us that the 200 repititions tend to provide similar (but not the same) histograms.
# When a mean of each of these replications is taken and compared to the original mean(mean of the original wind data)
# they turn out to be good approximations of the true value. This shows the principle of law of convergence of large numbers
# and the central limit theorem holds true for our sampling cases. 
# The central limit theorem is an extension of law of convergence of large numbers that is, as the number of repitions is increased
# the mean of the replicated sample converges towards the true mean value. This is observed in our tests as well.
