################################################
# IST 687, HW03
#
# Student name:Yesaswi Avula
# Homework number: HW03

#
# Attribution statement: (choose the statements that are true)
# 1. I did this work with help from the book and the professor 
#    and these Internet sources: <https://www.datamentor.io/r-programming/histogram/>
#

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# Funcation to clean the census data
readStates <- function(inputDf){
  
  # Removing unnecessary rows and columns
  inputDf <- inputDf[-1:-8,] # First 8 rows contain info about US region census and not state census. Hence removed
  inputDf <- inputDf[,1:5] # The last 5 columns don't contain any information. Hence removed
  inputDf <- inputDf[-52:-58,] # The last 7 rows contains information about puerto rico and publishing information. Hence removed.
  
  # Renaming the column names into names useful for interpretation
  column_names <- c('stateName', 'Census',  'Estimated', 'Pop2010', 'Pop2011')
  colnames(inputDf) <- column_names # Assigning new column names to inputDf
  colnames(inputDf) # Printing new column names
  
  # Cleaning state names. Removing "." before the state names
  inputDf$stateName <- gsub("\\.", "", inputDf$stateName)
  
  # Removing "," from the numeric values stored as strings
  inputDf$Census <- gsub(",", "", inputDf$Census)
  inputDf$Estimated <- gsub(",", "", inputDf$Estimated)
  inputDf$Pop2010 <- gsub(",", "", inputDf$Pop2010)
  inputDf$Pop2011 <- gsub(",", "", inputDf$Pop2011)
  
  # Coercing numeric strings to numeric values
  inputDf$Census <- as.numeric(gsub(" ", "", inputDf$Census))
  inputDf$Estimated <- as.numeric(gsub(" ", "", inputDf$Estimated))
  inputDf$Pop2010 <- as.numeric(gsub(" ", "", inputDf$Pop2010))
  inputDf$Pop2011 <- as.numeric(gsub(" ", "", inputDf$Pop2011))
  
  rownames(inputDf) <- NULL # Resetting the row index
  
  View(inputDf) # Viewing cleaned DF
  
  return(inputDf) # Returning cleaned inputDF
  
}


# Reading the census data
census_input <- read.csv('Nst-est2011.csv') # The census CSV is present in the local directory
dfStates <- readStates(census_input)
View(dfStates)

# Calculating Min, Max, Mean population referencing Pop2011
min_pop_index <- which.min(dfStates$Pop2011) # Min population index
min_pop_index # Row with the minimum population
min_pop <- min(dfStates$Pop2011) # Minimum Population
min_pop # Printing Minimum Popultation
dfStates[min_pop_index,"stateName"] # Printing state with Minimum Population

max_pop_index <- which.max(dfStates$Pop2011) # Max population index
max_pop_index # Row with the maximum population
max_pop <- max(dfStates$Pop2011) # Maximum Population
max_pop # Printing Maximum population
dfStates[max_pop_index,"stateName"] # Printing state with Maximum population

mean_pop <- mean(dfStates$Pop2011) # Mean population across the US
mean_pop # Printing mean population

# Sorting the dfStates DF by 2011 population
dfStatesOrdered <- dfStates[order(dfStates$Pop2011),]
rownames(dfStatesOrdered) <- NULL # Resetting the row index of new DF
View(dfStatesOrdered) # Viewing States ordered by population

# Histogram of 2011 population. Distribution of US population
hist(dfStatesOrdered$Pop2011, breaks = 20, main = "2011 Population Histogram", xlab = "Population", ylab = "Count/Frequency") # 20 bins histogram to represent