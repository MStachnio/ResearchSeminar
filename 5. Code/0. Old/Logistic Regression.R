library(xts)
library(zoo)

# Date: 16.03.2018
# Authors: Michal Stachnio & Jérôme Voelke

# To-Do log:
# 1) Normalize/convert data into calculable form (i.e. see paper technical indicators p. 5313) (create a function to do that !)
# 2) Create the "Testing" part
# 3) Tie-up part 2 and part 3 (data manip and training)
# 
# Personal notes: 
# 1) What's nice about logistic regression is that we can decide to invest only on those days we have 99.9% certainty. 
# 2) We need to make sure there is no tail risk. i.e. if you invest with 99.9% certainty and you earn only 1.- but with 0.01% chance you loose 1m.-, it's not great


#
#
#         Part 1: Creating the dataFrame
#
#


# Opening the core data file
csvFilePathCoreData = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/New_Data.csv"
coreData = read.csv(file = csvFilePathCoreData, # path of the file
                    header = TRUE, # include the headers
                    sep = ",") # How the data is separated
coreData[coreData == ""] <- NA # replace all empty spaces with NA

# OPTIONAL: Truncating the size for testing
coreData = coreData[1:20, c('Time', 'SP_500', 'SP_500_Volume')]

# Create a Timeseries of tedRate indexed by date
coreData$Time = as.Date(coreData$Time, format = "%d.%m.%Y") # Bug: output pour les années c'est 0010 à la place de 2010.
coreData = xts(coreData[, 2:3], order.by = coreData$Time)




#
#
#         Part 2: Data Manipulation
#
#


# Creating the S&P evolution vector, a vector with binary data (1 if the index value increases, 0 otherwise)
# Personal ToDo: if I have time I could define this as a function
BinominalEvolution = function (x){
  for (i in 1:length(x)) {
    if (i == length(x)) { # When we get to the last data entry, the output is always NA because we cannot compare it to a future value. Without this, there is an error message
      x[i, ] = NA
    } else if (is.na(x[i+1, ]) || is.na(x[i, ])) { # If comparing with a NA, the output is automatically NA.
      x[i, ] = NA
    } else if (coredata(x[i+1, ]) > coredata(x[i, ])) {
      x[i, ] = 1
    } else {
      x[i, ] = 0
    }
  }
  return (x)
}
spEvolution = BinominalEvolution(coreData$SP_500)
colnames(spEvolution) = "spEvolution"

# Calculating the simple 10-day moving average of the S&P500 index
# note: rollmean(coreData$SP_500, k = 10) does not give the desired output.
SimpleMovingAverageTS = function (x, n) {
  divisor = n
  y = x
  for (l in 1:length(x)) { # This part calculates the simple sum
    weightedSum = 0
    if (l < n) {
      y[l,] = NA
    } else {
      for (i in 1:n) {
        weightedSum = weightedSum + coredata(x[l - i + 1])
      }
      y[l, ] = weightedSum
    }
  }
  return(y/divisor) # The function outputs a weighted timeseries
}
spSimpleMovingAverage = SimpleMovingAverageTS(coreData$SP_500, 10)
colnames(spSimpleMovingAverage) = "spSimpleMovingAverage"

# Calculating the weighted 10-day moving average of the S&P 500 index
# We need to define a function for this purpose
WeightedMovingAverageTS = function (x, n) { # The function takes two inputs: x = the timeseries that is weighted, n = the amount of days of the weighting
  divisor = 0
  y = x
  for (q in 1:n) { # This part calculates the divisor (i.e. the denominator of the weighted sum)
    divisor = divisor + q
  }
  for (l in 1:length(x)) { # This part calculates the weighted sum
    weightedSum = 0
    if (l < n) {
      y[l, ] = NA
    } else {
      for (i in 1:n) {
        weightedSum = weightedSum + coredata(x[l - i + 1]) * (n - i + 1)
      }
      y[l, ] = weightedSum
    }
  }
  return (y/divisor) # The function outputs a weighted timeseries
}
spWeightedAverage = WeightedMovingAverageTS(coreData$SP_500, 10)
colnames(spWeightedAverage) = "spWeightedAverage"

#
#
#         Part 3: Logistical Regression
#
#


# Initial parameters
randomWeight = 0.00005 # The value of the initial weights. note: we can randomize this later on
attributeVariables = merge(spSimpleMovingAverage, spWeightedAverage) # matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
resultVector = spEvolution # rbinom(10, 1, 0.5) # !!!!!!!!! Need to define this from the data !!!!!!!!!!
outerloop = 1 # How many times we go over the data
eta = 0.1 # error adaptation coefficient
flagPerceptronLearning = 0 # 0 if using an ADALINE-like learning algorithm, 1 if using an PERCEPTRON-like learning algorithm.


# Creating the x_0 vector
x_0 = xts(rep(1, length(spEvolution)), order.by = index(coreData))

# Merging all the data into the dataFrame
dataSet = merge(x_0, attributeVariables, resultVector)

# VERY IMPORTANT: TAKING OUT ALL THE ROWS WITH NAs
dataSet = dataSet[complete.cases(dataSet), ]

# Key variables for calculations
numberAttributeVariables = ncol(dataSet) - 2
numberObservations = nrow(dataSet)

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




# Defining the key variables for the Logistic Regression Algorithm
index = matrix(0, 1, numberObservations * outerloop)
indexPrediction = 0 
weightMatrix = matrix(NA, nrow(weightVector),  numberObservations * outerloop) # WeightMatrix is used to record the weights through the various iterations.
weightMatrix[,1] = weightVector
rownames(weightMatrix) = c(weightRowNames)
error = 0
predictionAccuracy = matrix(NA, 1,  numberObservations * outerloop)
lastError = 0 
l = 1
i = 1

# Core Algorithm: reweighting based on observation data
for (l in 1 : outerloop) {
  for (i in 1 : numberObservations) {
     weightMatrix [,i + (l - 1) * numberObservations] = weightVector
    index[i] = 1/(1 + exp(-(sum(coredata(dataSet[i,0 : numberAttributeVariables + 1]) %*% weightVector ))))
    if (index[i] < 0.5) {
      indexPrediction[i] = 0
     } else {indexPrediction[i] = 1
    }
    error[i] = dataSet[i, numberAttributeVariables + 2] - flagPerceptronLearning * indexPrediction[i] + (-1 + flagPerceptronLearning) * index[i] # flagPerceptronLearning defines whether we use an ADALINE or Perceptron-like algorithm
    # Also tests whether our prediction was accurate and stores it in predictionAccuracy
    if ((index[i] < 0.5) & (dataSet[i, numberAttributeVariables + 2] < 0.5) | (( index[i]) >= 0.5) & (dataSet[i, numberAttributeVariables + 2] >= 0.5)) {
      predictionAccuracy[, i + (l - 1) * numberObservations] = 1
      } else {
        predictionAccuracy[, i + (l - 1) * numberObservations] = 0
        lastError = i + (l - 1) * numberObservations
      }
    weightVector =  weightVector + eta * error[i] * index[i] * (1 - index[i]) * as.vector(dataSet[i, 0 : numberAttributeVariables + 1])
  }
}
                
                