################################################
# IST 687, HW01
#
# Student name:Yesaswi Avula
# Homework number:HW01
# 
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work with help from the book and the professor
#    and these Internet sources <https://www.datamentor.io/r-programming/if-else-statement/>
# Defining vectors to store the heights and weights of members of a particular team
# Units
# Height: Inches
# Weight: Pounds

# Run these three functions to get a clean test of homework code
# dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

height<-c(59, 60, 61, 58, 67, 72, 70)# Heights of the players
weight<-c(150, 140, 180, 220, 160, 140,130) # Weights of the players

a<-150 # Defining a variable 'a' and storing and constant in it

#Calculating the Mean of the players height and Weight
avgHeight<-mean(height)
avgWeight<-mean(weight)

avgHeight # Displaying the mean height
avgWeight # Displaying the mean weight

# Calculating and Displaying the number of observations in the height and weight variables
numHeight<-length(height)
numWeight<-length(weight)

numHeight # Number of observations in height vector
numWeight # Number of observations in weight vector

# Calculating and Displaying the total height and weight of the tema members
sumHeight<-sum(height)
sumWeight<-sum(weight)

sumHeight # Displaying total Height
sumWeight # Displaying total Weight

# Manually calculating the teams mean Height and Weight

manAvgHeight<-sumHeight/numHeight
manAvgWeight<-sumWeight/numWeight

# Displaying the Manually Calculated Mean Height and Weight of the team members
manAvgHeight
manAvgWeight

# Maximum Height and Minimum Weight in the team
maxH<-max(height) # Storing Maximum height in the team
minW<-min(weight) # Storing Minimum weight in the team

# Displaying Maximum Height and Minimum Weight
maxH
minW

# Creating extra weight vector
extraWeight<-weight+25 # Adding extra weight of 25 to everyone on the team
extraWeight # Displaying the extraWeight vector

# Average weight of extraWeight vector
avgExtraWeight<-mean(extraWeight) # Computing the mean of extraWeight vector
avgExtraWeight # Displaying the avergae of extraWeight

# Creating a variable and assigning it test value to avoid use of constant
testH<-70 # A value to test against max height

# Check if maxH greater than the testH i.e. 70
if (maxH > testH) {
  "yes, Max Height greater than test value"
} else {
  "no, Max height NOT greater than test value"
}

# Check if minW is greater than variable "a"
if (minW > a){
  "yes, Minimum Weight greater than a"
} else {
  "no, Minimum weight NOT greater than a"
}

# Store values which are greater than 60 in height vector
height>60 # Generating a boolean list to check which elements in vector satisfy condition
bigHT<-height[height>60] # bigHT contains all heights greater than 60
bigHT # Displaying bigHT

# Store 2 & 4 elements from the weight vector
smallWT<-weight[c(2,4)] # Concatinating a list with required indexes and using it extract required elements
smallWT # Displaying the smallWT vector

# Removing third element and trying to store it back
weight<-weight[-3] # Third element is removed but not stored back in weight
weight # Displaying the weight vector with 3rd element removed

height(3) # Generates an error beacuse there is no function defined height. 
# If we are trying to index then "[]" instead of "()"