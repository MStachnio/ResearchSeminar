library(xts)
library(zoo)

#
#
#         Part 1: Creating the dataFrame
#
#

# Opening the core data file
csvFilePathCoreData = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/New_Data.csv"
coreData = read.csv(file=csvFilePathCoreData, header=TRUE, sep=",")
coreData[coreData == ""] <- NA # replace all empty spaces with NA
# OPTIONAL: Truncating the size for testing
coreData = coreData[1:20, c('Time', 'SP_500', 'SP_500_Volume')]
# Create a Timeseries of tedRate indexed by date
coreData$Time = as.Date(coreData$Time, format="%d.%m.%Y") # Bug: output pour les années c'est 0010 à la place de 2010.
coreData = xts(coreData[,2:3], order.by = coreData$Time)


# # Opening the data
# csvFilePathTedRate = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/TED Spread.csv"
# tedRate = read.csv(file=csvFilePathTedRate, header=TRUE, sep=",")
# 
# # Replaces all "." as "NA"
# tedRate[tedRate == "."] <- NA
# 
# # Creates a sequence of dates
# startingDate = "2000-01-01"
# endDate = "2018-03-05"
# dates = seq(as.Date(startingDate), as.Date(endDate), by=1)
# 
# # OPTIONAL: Truncating the size for testing
# tedRate = tedRate[1:20, c('DATE', 'TEDRATE')]
# dates = dates[1:29]
# 
# # Create a Timeseries of tedRate indexed by date
# tedRate$DATE = as.Date(tedRate$DATE, format="%Y-%m-%d")
# tedRate = xts(tedRate[,2], order.by = tedRate$DATE)
# 
# #Converts the data into numeric value (somehow xts automatically converts it into characters)
# storage.mode(tedRate) <- "numeric"
# 
# tedRate2 = lag(tedRate, k=1, na.pad =FALSE)
# merge(tedRate, tedRate2, all=TRUE, fill = NA)

#
#
#         Part 2: Data Manipulation
#
#

# Creating the S&P evolution vector



# length(tedRate)
# 
# tedRateEvolution = tedRate
# i=1
# # Creates "tedRateEvolution", it's a vector with binary data (1 if the value increases at t+1, and 0 otherwise).
# for (i in 1:length(tedRate)) {
#   if (i==length(tedRate)) { # When we get to the last data entry, the output is always NA because we cannot compare it to a future value. Without this, there is an error message
#     tedRateEvolution[i, ] = NA
#   } else if (is.na(tedRate[i+1, ]) || is.na(tedRate[i, ])) { # If comparing with a NA, the output is automatically NA.
#     tedRateEvolution[i, ] = NA
#   } else if (coredata(tedRate[i+1, ]) > coredata(tedRate[i, ])) {
#     tedRateEvolution[i, ] = 1
#   } else {
#     tedRateEvolution[i, ] = 0
#   }
# }



#
#
#         Part 3: Logistical Regression
#
#

# Personal notes: 
# 1) What's nice about logistic regression is that we can decide to invest only on those days we have 99.9% certainty. 
# 2) We need to make sure there is no tail risk. i.e. if you invest with 99.9% certainty and you earn only 1.- but with 0.01% chance you loose 1m.-, it's not great

# Initial parameters
randomWeight = 0.5 # The value of the initial weights. note: we can randomize this later on
attributeVariables = matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
resultVector = rbinom(10, 1, 0.5) # !!!!!!!!! Need to define this from the data !!!!!!!!!!
outerloop = 3 # How many times we go over the data
eta = 0.1 # error adaptation coefficient
flagPerceptronLearning = 0 # 0 if using an ADALINE-like learning algorithm, 1 if using an PERCEPTRON-like learning algorithm.

# Key variables
numberAttributeVariables = nrow(attributeVariables)
numberObservations = ncol(attributeVariables)

# Creating the weight vector
weightVector = matrix(randomWeight, # define the vector of weights
                      nrow = numberAttributeVariables+1, # we Create a weight for each dependent variable, plus one for w0.
                      ncol= 1,
                      byrow = TRUE
                      )

# naming the rows in weightVector
weightRowNames = matrix("0", nrow = numberAttributeVariables+1, ncol= 1, byrow = TRUE)
for (i in 1:nrow(weightRowNames)) {
  weightRowNames[i, ] = paste("w", i-1, 
                        sep="") # sep="" means that there is no space between w and the number
}
rownames(weightVector)=c(weightRowNames)


# Creating the x vector
x_0 = matrix(1, nrow = 1, ncol = numberObservations)
dataSet = matrix(c(x_0, attributeVariables, resultVector), nrow = 2 + numberAttributeVariables, ncol = numberObservations, byrow = TRUE)

# Assigns the right names to rows in dataSet
dataSetRowNames = matrix("0", nrow = nrow(dataSet), ncol= 1, byrow = TRUE)
for (i in 1:nrow(dataSet)) {
  if (i == 1) {
    dataSetRowNames[i, ] = "x0"
  } else if (i < nrow(dataSet)) {
    dataSetRowNames[i, ] = paste("x", i - 1, sep = "") # sep = "" means that there is no space between w and the number
  } else {
    dataSetRowNames[i, ] = "y"
  }
}
rownames(dataSet) = c(dataSetRowNames)


# Defining the key variables for the Logistic Regression Algorithm
index = matrix(0, 1, numberObservations * outerloop)
indexPrediction = 0 
weightMatrix = matrix(NA, nrow(weightVector),  numberObservations * outerloop) # WeightMatrix is used to record the weights through the various iterations.
weightMatrix[,1] = weightVector
rownames(weightMatrix) = c(weightRowNames)
error = 0
predictionAccuracy = matrix(NA, 1,  numberObservations * outerloop)
lastError = 0 

# Core Algorithm: reweighting based on observation data
for (l in 1:outerloop){
  for (i in 1 : numberObservations){
     weightMatrix[,i + (l - 1) * numberObservations] = weightVector
    index[i] = 1/(1 + exp(-(sum(weightVector * dataSet[0 : numberAttributeVariables + 1, i]))))
    if (index[i] < 0.5) {
      indexPrediction[i] = 0
     } else {indexPrediction[i] = 1
    }
    error[i] = dataSet[numberAttributeVariables + 2, i] - flagPerceptronLearning * indexPrediction[i] + (-1 + flagPerceptronLearning) * index[i] # flagPerceptronLearning defines whether we use an ADALINE or Perceptron-like algorithm
    # Also tests whether our prediction was accurate and stores it in predictionAccuracy
    if ((index[i] < 0.5) & (dataSet[numberAttributeVariables + 2, i] < 0.5) | (( index[i]) >= 0.5) & (dataSet[numberAttributeVariables + 2, i] >= 0.5)) {
      predictionAccuracy[, i + (l - 1) * numberObservations] = 1
      } else {
        predictionAccuracy[, i + (l - 1) * numberObservations] = 0
        lastError = i + (l - 1) * numberObservations
      }
    weightVector =  weightVector +  eta * error[i] * index[i] * (1 - index[i]) * dataSet[0 : numberAttributeVariables + 1, i]
  }
}
                
                