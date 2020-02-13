################################################
# IST 687, HW02
#
# Student name: Yesaswi Avula
# Homework number:HW02

#
# Attribution statement:
# 1. I did this work with help from the book and the professor

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Loading US Arrests dataset into a dataframe
myArrests<-USArrests # Creating Dataframe containing US Arrest Statistics
str(myArrests) # Structure of the myArrests DF
summary(myArrests) # Summary stats of myArrests DF
View(myArrests) # Displaying the dataframe


# Exploring Assault Rates in the USA
# A lower Assault rate is a good indicator about the safety of an area.
# Lower the assault rate safer the area. Lower Assault rate is BEST
# Assault rate is not the only determining factor in the safety of a state
# The mean assault rate is 170.8 (per 100,000) determined from summary(myArrests)
meanAssault<-mean(myArrests$Assault) # Calculating Mean Assault
meanAssault

head(myArrests[order(myArrests$Assault),],3) # Ordering the DF by ascending assault rates
head(myArrests[order(myArrests$Assault),],1) # lowest assault rate
# North Dakota has the lowest Assault rate in the USA. 
# Therefore the best Assault rate is in the state of North Dakota
# It is best because of our earlier assumption that lower assault rate is better


# Exploring the Murder rates
head(myArrests[order(myArrests$Murder, decreasing = TRUE ),], 3) # Ordering the DF by descending murder rate
head(myArrests[order(myArrests$Murder, decreasing = TRUE ),], 1) # highest murder rate
# Georgia has the highest murder rate

# Creating new DF with descending order of murder rates 
descMurder<-myArrests[order(myArrests$Murder, decreasing = TRUE),]
View(descMurder)
head(descMurder, 10) # Top 10 States with highest murder rate

# The Murder, Assault and Rape rates together are appropriate to determine the safety of a state
# Since the crime rates are per 100,000 an AVERAGE/MEAN of the three rates will be an accurate determiner
# for the safety index for states
# The other way to determine the safety index is a simple SUM of the three crimes rates
myArrests$safetyIndex<-myArrests$Murder+myArrests$Assault+myArrests$Rape # Sum of crime rates as Safety Index
View(myArrests) # Viewing the DF with safety index column
safeStates<-myArrests[order(myArrests$safetyIndex),] # Ordering DF by safety index
View(safeStates)
head(safeStates,1)
# Safest state is north dakota with safety index at 53.1

# Computing safety index using scale and adding weights to murder and rape crime rates
myArrests2 <- USArrests # New DF with US Arrests
myArrests2
# Scaling crime rates. Murder and Rape rates are given twice the weight as Assault rates
myArrests2$safetyIndex<- 2*scale(myArrests2$Murder)+2*scale(myArrests2$Rape)+1*scale(myArrests2$Assault)
View(myArrests2)
head(myArrests2[order(myArrests2$safetyIndex),],5) # 5 safest states based on new safety index

# The new safety index using scale() function is a better metric to determine safety of states
# when compared to the earlier method of SUM of different crime rates. 
# Adding weight to the different crime rates is essential because when observing the data the number of assaults
# are far higher than the rape and murder rates in most states which will influence the safety index immensely if
# only a simple SUM/MEAN is taking for determining the safety index.
# The scale function normalizes across different crime rates by computing individual mean and Std. deviation and 
# then scaling the values so that the different crime rates maybe comparable. Normalizing different crime rates
# is essential if we are trying to compare them which wasn't being achieved in the first computation of the safety Index.
